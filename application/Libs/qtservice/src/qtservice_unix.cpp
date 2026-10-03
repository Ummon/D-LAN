/****************************************************************************
**
** Copyright (C) 2013 Digia Plc and/or its subsidiary(-ies).
** Contact: http://www.qt-project.org/legal
**
** This file is part of the Qt Solutions component.
**
** $QT_BEGIN_LICENSE:BSD$
** You may use this file under the terms of the BSD license as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of Digia Plc and its Subsidiary(-ies) nor the names
**     of its contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "qtservice.h"
#include "qtservice_p.h"
#include <QCoreApplication>
#include <QStringList>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QProcess>
#include <QSocketNotifier>
#include <errno.h>
#include <pwd.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <signal.h>

// On Linux the service is a systemd unit named after the service: "<service name>.service".
// systemd runs the executable as a regular application, without any service specific argument.
// There is no service on the other Unix systems (macOS, ..): nothing is installed and nothing can be controlled.

static const char SYSTEMD_UNIT_DIRECTORY[] = "/etc/systemd/system";

static bool systemdAvailable()
{
#if defined(Q_OS_LINUX)
    // The same test as sd_booted(3).
    return QFileInfo(QLatin1String("/run/systemd/system")).isDir();
#else
    return false;
#endif
}

static QString unitName(const QString &serviceName)
{
    return serviceName + QLatin1String(".service");
}

static QString unitFilePath(const QString &serviceName)
{
    return QLatin1String(SYSTEMD_UNIT_DIRECTORY) + QLatin1Char('/') + unitName(serviceName);
}

// Runs systemctl and waits for its end, its output isn't captured.
// Without the required rights it fails instead of asking for a password: it's also run by a GUI.
static bool systemctl(const QStringList &arguments)
{
    return systemdAvailable()
        && QProcess::execute(QLatin1String("systemctl"), QStringList(QLatin1String("--no-ask-password")) + arguments) == 0;
}

// Returns the value of the first line "<key>=<value>" of the unit file.
static QString unitFileValue(const QString &serviceName, const QString &key)
{
    QFile file(unitFilePath(serviceName));
    if (!file.open(QIODevice::ReadOnly))
        return QString();

    const QString prefix = key + QLatin1Char('=');
    while (!file.atEnd()) {
        const QString line = QString::fromUtf8(file.readLine()).trimmed();
        if (line.startsWith(prefix))
            return line.mid(prefix.size());
    }
    return QString();
}

// Quotes an argument of a command line of a unit file, see "Command lines" in systemd.service(5).
static QString quoted(QString argument)
{
    argument.replace(QLatin1Char('\\'), QLatin1String("\\\\"));
    argument.replace(QLatin1Char('"'), QLatin1String("\\\""));
    argument.replace(QLatin1Char('%'), QLatin1String("%%"));
    return QLatin1Char('"') + argument + QLatin1Char('"');
}

static QString absPath(const QString &path)
{
    QString ret;
    if (path[0] != QChar('/')) { // Not an absolute path
        int slashpos;
        if ((slashpos = path.lastIndexOf('/')) != -1) { // Relative path
            QDir dir = QDir::current();
            dir.cd(path.left(slashpos));
            ret = dir.absolutePath();
        } else { // Need to search $PATH
            char *envPath = ::getenv("PATH");
            if (envPath) {
                QStringList envPaths = QString::fromLocal8Bit(envPath).split(':');
                for (int i = 0; i < envPaths.size(); ++i) {
                    if (QFile::exists(envPaths.at(i) + QLatin1String("/") + QString(path))) {
                        QDir dir(envPaths.at(i));
                        ret = dir.absolutePath();
                        break;
                    }
                }
            }
        }
    } else {
        QFileInfo fi(path);
        ret = fi.absolutePath();
    }
    return ret;
}

static QString executablePath(const QStringList &args)
{
    if (args.isEmpty())
        return QString();
    QFileInfo fi(args[0]);
    QDir dir(absPath(args[0]));
    return dir.absoluteFilePath(fi.fileName());
}

// Returns the AppImage the executable is run from, an empty string if it isn't run from an AppImage.
// The executable is then in a temporary mount: the service has to run the AppImage itself.
static QString appImagePath(const QString &executable)
{
    const QString appImage = QFile::decodeName(qgetenv("APPIMAGE"));
    const QString appDir = QFileInfo(QFile::decodeName(qgetenv("APPDIR"))).canonicalFilePath();
    // These variables may be inherited from another application run from an AppImage.
    if (appImage.isEmpty() || appDir.isEmpty()
        || !QFileInfo(executable).canonicalFilePath().startsWith(appDir + QLatin1Char('/')))
        return QString();
    return appImage;
}

QString QtServiceBasePrivate::filePath() const
{
    const QString executable = executablePath(args);
    const QString appImage = appImagePath(executable);
    return appImage.isEmpty() ? executable : appImage;
}


QString QtServiceController::serviceDescription() const
{
    // '%' is doubled by install().
    return unitFileValue(serviceName(), QLatin1String("Description")).replace(QLatin1String("%%"), QLatin1String("%"));
}

QtServiceController::StartupType QtServiceController::startupType() const
{
    if (isInstalled() && systemctl(QStringList() << QLatin1String("is-enabled") << QLatin1String("--quiet") << unitName(serviceName())))
        return AutoStartup;
    return ManualStartup;
}

QString QtServiceController::serviceFilePath() const
{
    // The command line is written by install() as: "<file path>" [<arguments>]. See quoted(..).
    const QString command = unitFileValue(serviceName(), QLatin1String("ExecStart"));
    if (!command.startsWith(QLatin1Char('"')))
        return command.section(QLatin1Char(' '), 0, 0);

    QString path;
    for (int i = 1; i < command.size() && command.at(i) != QLatin1Char('"'); ++i) {
        // Skip the character added by quoted(..).
        if ((command.at(i) == QLatin1Char('\\') || command.at(i) == QLatin1Char('%')) && i + 1 < command.size())
            ++i;
        path += command.at(i);
    }
    return path;
}

bool QtServiceController::uninstall()
{
    if (!isInstalled())
        return false;

    // Stop the service and don't start it at boot anymore.
    // It may fail because the service is neither running nor started at boot.
    systemctl(QStringList() << QLatin1String("disable") << QLatin1String("--now") << unitName(serviceName()));

    const QString path = unitFilePath(serviceName());
    if (!QFile::remove(path)) {
        fprintf(stderr, "Cannot uninstall \"%s\". Cannot remove: %s. Check permissions.\n",
                serviceName().toLatin1().constData(),
                path.toLatin1().constData());
        return false;
    }

    systemctl(QStringList(QLatin1String("daemon-reload")));
    return true;
}


bool QtServiceController::start(const QStringList &arguments)
{
    // The command line of a unit can't be changed when it's started.
    Q_UNUSED(arguments)
    return isInstalled() && systemctl(QStringList() << QLatin1String("start") << unitName(serviceName()));
}

bool QtServiceController::stop()
{
    return isInstalled() && systemctl(QStringList() << QLatin1String("stop") << unitName(serviceName()));
}

bool QtServiceController::pause()
{
    return false;
}

bool QtServiceController::resume()
{
    return false;
}

bool QtServiceController::sendCommand(int code)
{
    Q_UNUSED(code)
    return false;
}

bool QtServiceController::isInstalled() const
{
    return systemdAvailable() && QFile::exists(unitFilePath(serviceName()));
}

bool QtServiceController::isRunning() const
{
    return isInstalled() && systemctl(QStringList() << QLatin1String("is-active") << QLatin1String("--quiet") << unitName(serviceName()));
}




///////////////////////////////////

static int stopPipe[2] = { -1, -1 };

static void stopSignalHandler(int)
{
    // Only async-signal-safe functions can be called here: the service is stopped from the event loop.
    const int savedErrno = errno;
    const char byte = 0;
    const ssize_t written = ::write(stopPipe[1], &byte, 1);
    Q_UNUSED(written);
    errno = savedErrno;
}

// Stop the service properly when SIGTERM is received: it's sent by systemd to stop a service.
void QtServiceBasePrivate::installStopSignalHandler()
{
    if (::pipe(stopPipe) != 0)
        return;
    ::fcntl(stopPipe[0], F_SETFD, FD_CLOEXEC);
    ::fcntl(stopPipe[1], F_SETFD, FD_CLOEXEC);

    QSocketNotifier *notifier = new QSocketNotifier(stopPipe[0], QSocketNotifier::Read, QCoreApplication::instance());
    QObject::connect(notifier, &QSocketNotifier::activated, notifier, [this, notifier]() {
        notifier->setEnabled(false);
        stopService();
    });

    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = stopSignalHandler;
    sigemptyset(&action.sa_mask);
    // The default action is restored when the signal is received: a second signal kills the process.
    action.sa_flags = SA_RESTART | SA_RESETHAND;
    ::sigaction(SIGTERM, &action, 0);
}

void QtServiceBasePrivate::stopService()
{
    q_ptr->stop();
    QCoreApplication::quit();
}

// The process is never run as a service on Unix, see the top of this file.
bool QtServiceBasePrivate::sysInit()
{
    return true;
}

void QtServiceBasePrivate::sysSetPath()
{
}

void QtServiceBasePrivate::sysCleanup()
{
}

bool QtServiceBasePrivate::start()
{
    if (!controller.isInstalled()) {
        fprintf(stderr, "The service %s is not installed\n", controller.serviceName().toLatin1().constData());
        return false;
    }
    return controller.start();
}

QString QtServiceBasePrivate::installationError() const
{
#if defined(Q_OS_LINUX)
    if (!systemdAvailable())
        return QLatin1String("The service requires systemd, it is not running on this system");
    if (::access(SYSTEMD_UNIT_DIRECTORY, W_OK) != 0)
        return QString::fromLatin1("Administrator rights are required to install or uninstall the service, cannot write to: %1")
            .arg(QLatin1String(SYSTEMD_UNIT_DIRECTORY));
    return QString();
#else
    return QLatin1String("The service is not supported on this system");
#endif
}

QString QtServiceBasePrivate::installationNotice(bool install) const
{
    return QString::fromLatin1(install ? "A systemd unit '%1' will be created in %2" : "The systemd unit '%1' will be stopped and removed from %2")
        .arg(unitName(controller.serviceName()), QLatin1String(SYSTEMD_UNIT_DIRECTORY));
}

bool QtServiceBasePrivate::install(const QString &account, const QString &password)
{
    Q_UNUSED(password)

    // The user is always given: systemd only defines its home directory, where the data are put, in this case.
    QString user = account;
    if (user.isEmpty())
        user = QLatin1String("root");
    if (!::getpwnam(user.toLocal8Bit().constData())) {
        fprintf(stderr, "Cannot install \"%s\". Unknown account: %s.\n",
                controller.serviceName().toLatin1().constData(),
                user.toLocal8Bit().constData());
        return false;
    }

    QString description = serviceDescription.isEmpty() ? controller.serviceName() : serviceDescription;
    description.replace(QLatin1Char('\n'), QLatin1Char(' '));
    description.replace(QLatin1Char('%'), QLatin1String("%%"));

    QString command = quoted(filePath());
    // An AppImage of D-LAN runs the GUI by default, see its entry point in 'build.nu'.
    if (!appImagePath(executablePath(args)).isEmpty())
        command += QLatin1String(" --core");

    // 'KillMode': only the main process must receive a signal. An AppImage has a second process which mounts
    // its content: stopped at the same time, the executable would disappear during its shutdown, and killed
    // right after, the mount would be left behind. This process ends by itself after the main one.
    const QByteArray unit = QString::fromLatin1(
        "[Unit]\n"
        "Description=%1\n"
        "Wants=network-online.target\n"
        "After=network-online.target\n"
        "\n"
        "[Service]\n"
        "ExecStart=%2\n"
        "User=%3\n"
        "KillMode=process\n"
        "\n"
        "[Install]\n"
        "WantedBy=multi-user.target\n").arg(description, command, user).toUtf8();

    const QString path = unitFilePath(controller.serviceName());
    QFile file(path);
    if (!file.open(QIODevice::WriteOnly | QIODevice::NewOnly) || file.write(unit) != unit.size()) {
        fprintf(stderr, "Cannot install \"%s\". Cannot write to: %s. Check permissions.\n",
                controller.serviceName().toLatin1().constData(),
                path.toLatin1().constData());
        return false;
    }
    file.setPermissions(QFile::ReadOwner | QFile::WriteOwner | QFile::ReadGroup | QFile::ReadOther);
    file.close();

    if (!systemctl(QStringList(QLatin1String("daemon-reload")))) {
        QFile::remove(path);
        return false;
    }

    // Without being enabled the service is only started on demand.
    if (startupType == QtServiceController::AutoStartup)
        return systemctl(QStringList() << QLatin1String("enable") << unitName(controller.serviceName()));
    return true;
}

void QtServiceBase::logMessage(const QString &message, QtServiceBase::MessageType type,
			    int, uint, const QByteArray &)
{
    int st;
    switch(type) {
        case QtServiceBase::Error:
	    st = LOG_ERR;
	    break;
        case QtServiceBase::Warning:
            st = LOG_WARNING;
	    break;
        default:
	    st = LOG_INFO;
    }
    // 'ident' must stay valid until the log is closed.
    const QByteArray ident = serviceName().toLocal8Bit();
    openlog(ident.constData(), LOG_PID, LOG_DAEMON);
    const QStringList lines = message.split('\n');
    for (const QString &line : lines)
        syslog(st, "%s", line.toLocal8Bit().constData());
    closelog();
}

void QtServiceBase::setServiceFlags(QtServiceBase::ServiceFlags flags)
{
    d_ptr->serviceFlags = flags;
}
