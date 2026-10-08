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
#include <QMutex>
#include <QSemaphore>
#include <qt_windows.h>
#include <QWaitCondition>
#include <QVector>
#include <QThread>
#include <stdio.h>

// A handle of the service control manager or of a service, closed when it goes out of scope.
class ScHandle
{
public:
    ScHandle(SC_HANDLE handle) : handle(handle) {}
    ~ScHandle() { if (handle) CloseServiceHandle(handle); }
    operator SC_HANDLE() const { return handle; }

private:
    Q_DISABLE_COPY(ScHandle)
    SC_HANDLE handle;
};

// A service opened with the given access rights, null if it can't be opened.
class ServiceHandle
{
public:
    ServiceHandle(const QString &name, DWORD access, DWORD managerAccess = SC_MANAGER_CONNECT)
        : manager(OpenSCManagerW(0, 0, managerAccess)),
          service(manager ? OpenServiceW(manager, (const wchar_t *)name.utf16(), access) : 0)
    {}
    operator SC_HANDLE() const { return service; }

private:
    const ScHandle manager;
    const ScHandle service; // Closed before the manager.
};

bool QtServiceController::isInstalled() const
{
    Q_D(const QtServiceController);
    const ServiceHandle service(d->serviceName, SERVICE_QUERY_CONFIG);
    return service != 0;
}

bool QtServiceController::isRunning() const
{
    Q_D(const QtServiceController);
    const ServiceHandle service(d->serviceName, SERVICE_QUERY_STATUS);
    SERVICE_STATUS status;
    return service && QueryServiceStatus(service, &status) && status.dwCurrentState != SERVICE_STOPPED;
}

bool QtServiceController::uninstall()
{
    Q_D(QtServiceController);
    const ServiceHandle service(d->serviceName, DELETE|SERVICE_STOP|SERVICE_QUERY_STATUS, SC_MANAGER_ALL_ACCESS);
    if (!service) {
        if (GetLastError() == ERROR_ACCESS_DENIED)
            fprintf(stderr, "Administrator rights are required to uninstall the service\n");
        return false;
    }

    // A running service is only marked for deletion and stays registered
    // until it stops, so stop it first (wait up to 30 s).
    SERVICE_STATUS status;
    if (QueryServiceStatus(service, &status) && status.dwCurrentState != SERVICE_STOPPED) {
        if (status.dwCurrentState != SERVICE_STOP_PENDING)
            ControlService(service, SERVICE_CONTROL_STOP, &status);
        for (int i = 0; i < 150 && status.dwCurrentState != SERVICE_STOPPED; ++i) {
            Sleep(200);
            if (!QueryServiceStatus(service, &status))
                break;
        }
        if (status.dwCurrentState != SERVICE_STOPPED)
            fprintf(stderr, "The service could not be stopped, it will be removed once it stops\n");
    }
    return DeleteService(service) != 0;
}

bool QtServiceController::start(const QStringList &args)
{
    Q_D(QtServiceController);
    const ServiceHandle service(d->serviceName, SERVICE_START);
    if (!service)
        return false;

    QVector<const wchar_t *> argv(args.size());
    for (int i = 0; i < args.size(); ++i)
        argv[i] = (const wchar_t*)args.at(i).utf16();

    return StartServiceW(service, args.size(), argv.data()) != 0;
}

bool QtServiceController::stop()
{
    Q_D(QtServiceController);
    const ServiceHandle service(d->serviceName, SERVICE_STOP|SERVICE_QUERY_STATUS);
    if (!service)
        return false;

    SERVICE_STATUS status;
    if (!ControlService(service, SERVICE_CONTROL_STOP, &status)) {
        qErrnoWarning(GetLastError(), "stopping");
        return false;
    }
    for (int i = 0; i < 10 && status.dwCurrentState != SERVICE_STOPPED; ++i) {
        Sleep(200);
        if (!QueryServiceStatus(service, &status))
            break;
    }
    return status.dwCurrentState == SERVICE_STOPPED;
}

void QtServiceBase::logMessage(const QString &message, MessageType type,
                           int id, uint category, const QByteArray &data)
{
    Q_D(QtServiceBase);
    WORD wType;
    switch (type) {
    case Error: wType = EVENTLOG_ERROR_TYPE; break;
    case Warning: wType = EVENTLOG_WARNING_TYPE; break;
    case Information: wType = EVENTLOG_INFORMATION_TYPE; break;
    default: wType = EVENTLOG_SUCCESS; break;
    }
    HANDLE h = RegisterEventSourceW(0, (const wchar_t *)d->controller.serviceName().utf16());
    if (h) {
        const wchar_t *msg = (const wchar_t *)message.utf16();
        const char *bindata = data.size() ? data.constData() : 0;
        ReportEventW(h, wType, category, id, 0, 1, data.size(), &msg,
                     const_cast<char *>(bindata));
        DeregisterEventSource(h);
    }
}

class QtServiceControllerHandler : public QObject
{
    Q_OBJECT
public:
    QtServiceControllerHandler(QtServiceSysPrivate *sys);

protected:
    void customEvent(QEvent *e);

private:
    QtServiceSysPrivate *d_sys;
};

class QtServiceSysPrivate
{
public:
    enum {
        QTSERVICE_STARTUP = 256
    };
    QtServiceSysPrivate();

    void setStatus( DWORD dwState );
    void setServiceFlags(QtServiceBase::ServiceFlags flags);
    DWORD serviceFlags(QtServiceBase::ServiceFlags flags) const;
    static void WINAPI serviceMain( DWORD dwArgc, wchar_t** lpszArgv );
    static void WINAPI handler( DWORD dwOpcode );

    SERVICE_STATUS status;
    SERVICE_STATUS_HANDLE serviceStatus;
    QStringList serviceArgs;

    static QtServiceSysPrivate *instance;

    QWaitCondition condition;
    QMutex mutex;
    QSemaphore startSemaphore;
    QSemaphore startSemaphore2;

    QtServiceControllerHandler *controllerHandler;

    void handleCustomEvent(QEvent *e);
};

QtServiceControllerHandler::QtServiceControllerHandler(QtServiceSysPrivate *sys)
    : QObject(), d_sys(sys)
{

}

void QtServiceControllerHandler::customEvent(QEvent *e)
{
    d_sys->handleCustomEvent(e);
}


QtServiceSysPrivate *QtServiceSysPrivate::instance = 0;

QtServiceSysPrivate::QtServiceSysPrivate()
{
    instance = this;
}

void WINAPI QtServiceSysPrivate::serviceMain(DWORD dwArgc, wchar_t** lpszArgv)
{
    if (!instance || !QtServiceBase::instance())
        return;

    // Windows spins off a random thread to call this function on
    // startup, so here we just signal to the QApplication event loop
    // in the main thread to go ahead with start()'ing the service.

    for (DWORD i = 0; i < dwArgc; i++)
        instance->serviceArgs.append(QString::fromWCharArray(lpszArgv[i]));

    instance->startSemaphore.release(); // let the qapp creation start
    instance->startSemaphore2.acquire(); // wait until its done
    // Register the control request handler
    instance->serviceStatus = RegisterServiceCtrlHandlerW((const wchar_t *)QtServiceBase::instance()->serviceName().utf16(), handler);

    if (!instance->serviceStatus) // cannot happen - something is utterly wrong
        return;

    handler(QTSERVICE_STARTUP); // Signal startup to the application -
                                // causes QtServiceBase::start() to be called in the main thread

    // The MSDN doc says that this thread should just exit - the service is
    // running in the main thread (here, via callbacks in the handler thread).
}


// The handler() is called from the thread that called
// StartServiceCtrlDispatcher, i.e. our HandlerThread, and
// not from the main thread that runs the event loop, so we
// have to post an event to ourselves, and use a QWaitCondition
// and a QMutex to synchronize.
void QtServiceSysPrivate::handleCustomEvent(QEvent *e)
{
    int code = e->type() - QEvent::User;

    switch(code) {
    case QTSERVICE_STARTUP: // Startup
        QtServiceBase::instance()->start();
        break;
    case SERVICE_CONTROL_STOP:
        QtServiceBase::instance()->stop();
        QCoreApplication::instance()->quit();
        break;
    default:
        break;
    }

    mutex.lock();
    condition.wakeAll();
    mutex.unlock();
}

void WINAPI QtServiceSysPrivate::handler( DWORD code )
{
    if (!instance)
        return;

    instance->mutex.lock();
    switch (code) {
    case QTSERVICE_STARTUP: // QtService startup (called from WinMain when started)
        instance->setStatus(SERVICE_START_PENDING);
        QCoreApplication::postEvent(instance->controllerHandler, new QEvent(QEvent::Type(QEvent::User + code)));
        instance->condition.wait(&instance->mutex);
        instance->setStatus(SERVICE_RUNNING);
        break;
    case SERVICE_CONTROL_STOP: // 1
        instance->setStatus(SERVICE_STOP_PENDING);
        QCoreApplication::postEvent(instance->controllerHandler, new QEvent(QEvent::Type(QEvent::User + code)));
        instance->condition.wait(&instance->mutex);
        // status will be reported as stopped by start() when qapp::exec returns
        break;

    case SERVICE_CONTROL_INTERROGATE: // 4
        break;

    case SERVICE_CONTROL_SHUTDOWN: // 5
        // Don't waste time with reporting stop pending, just do it
        QCoreApplication::postEvent(instance->controllerHandler, new QEvent(QEvent::Type(QEvent::User + SERVICE_CONTROL_STOP)));
        instance->condition.wait(&instance->mutex);
        // status will be reported as stopped by start() when qapp::exec returns
        break;

    default:
        break;
    }

    instance->mutex.unlock();

    // Report current status
    if (instance->status.dwCurrentState != SERVICE_STOPPED)
        SetServiceStatus(instance->serviceStatus, &instance->status);
}

void QtServiceSysPrivate::setStatus(DWORD state)
{
    status.dwCurrentState = state;
    SetServiceStatus(serviceStatus, &status);
}

void QtServiceSysPrivate::setServiceFlags(QtServiceBase::ServiceFlags flags)
{
    status.dwControlsAccepted = serviceFlags(flags);
    SetServiceStatus(serviceStatus, &status);
}

DWORD QtServiceSysPrivate::serviceFlags(QtServiceBase::ServiceFlags flags) const
{
    DWORD control = 0;
    if (!(flags & QtServiceBase::CannotBeStopped))
        control |= SERVICE_ACCEPT_STOP;
    if (flags & QtServiceBase::NeedsStopOnShutdown)
        control |= SERVICE_ACCEPT_SHUTDOWN;

    return control;
}

#include "qtservice_win.moc"


class HandlerThread : public QThread
{
public:
    HandlerThread()
        : success(true), console(false)
        {}

    bool calledOk() { return success; }
    bool runningAsConsole() { return console; }

protected:
    bool success, console;
    void run()
        {
            SERVICE_TABLE_ENTRYW st [2];
            st[0].lpServiceName = (wchar_t*)QtServiceBase::instance()->serviceName().utf16();
            st[0].lpServiceProc = QtServiceSysPrivate::serviceMain;
            st[1].lpServiceName = 0;
            st[1].lpServiceProc = 0;

            success = (StartServiceCtrlDispatcherW(st) != 0); // should block

            if (!success) {
                if (GetLastError() == ERROR_FAILED_SERVICE_CONTROLLER_CONNECT) {
                    // Means we're started from console, not from service mgr
                    // start() will ask the mgr to start another instance of us as a service instead
                    console = true;
                }
                else {
                    QtServiceBase::instance()->logMessage(QString("The Service failed to start [%1]").arg(qt_error_string(GetLastError())), QtServiceBase::Error);
                }
                QtServiceSysPrivate::instance->startSemaphore.release();  // let start() continue, since serviceMain won't be doing it
            }
        }
};

/* There are three ways we can be started:

   - By a service controller (e.g. the Services control panel), with
   the -s(ervice) argument registered in the service binary path by install().
   ServiceBase::exec() will then call start() below, and the service will start.

   - From the console, with the -s(ervice) argument. This means we should
   ask a controller to start the service (i.e. another instance of this
   executable), and then just terminate. We discover this case (as
   different from the above) by the fact that StartServiceCtrlDispatcher
   will return an error, instead of blocking.

   - From the console, with no (service-specific) arguments.
   ServiceBase::exec() will then call ServiceBasePrivate::run(), which
   runs the application as a normal program.
*/

bool QtServiceBasePrivate::start()
{
    sysInit();

    // Since StartServiceCtrlDispatcher() blocks waiting for service
    // control events, we need to call it in another thread, so that
    // the main thread can run the QApplication event loop.
    HandlerThread* ht = new HandlerThread();
    ht->start();

    QtServiceSysPrivate* sys = QtServiceSysPrivate::instance;

    // Wait until service args have been received by serviceMain.
    // If Windows doesn't call serviceMain (or
    // StartServiceControlDispatcher doesn't return an error) within
    // a timeout of 20 secs, something is very wrong; give up
    if (!sys->startSemaphore.tryAcquire(1, 20000))
        return false;

    if (!ht->calledOk()) {
        if (ht->runningAsConsole())
            return controller.start(args.mid(1));
        else
            return false;
    }

    int argc = sys->serviceArgs.size();
    QVector<char *> argv(argc);
    QList<QByteArray> argvData;
    for (int i = 0; i < argc; ++i)
        argvData.append(sys->serviceArgs.at(i).toLocal8Bit());
    for (int i = 0; i < argc; ++i)
        argv[i] = argvData[i].data();

    q_ptr->createApplication(argc, argv.data());
    QCoreApplication *app = QCoreApplication::instance();
    if (!app)
        return false;

    sys->controllerHandler = new QtServiceControllerHandler(sys);

    sys->startSemaphore2.release(); // let serviceMain continue (and end)

    sys->status.dwWin32ExitCode = q_ptr->executeApplication();
    sys->setStatus(SERVICE_STOPPED);

    if (ht->isRunning())
        ht->wait(1000);         // let the handler thread finish
    delete sys->controllerHandler;
    sys->controllerHandler = 0;
    if (ht->isFinished())
        delete ht;
    delete app;
    sysCleanup();
    return true;
}

bool QtServiceBasePrivate::install(const QString &account, const QString &password)
{
    bool result = false;

    // Open the Service Control Manager
    const ScHandle hSCM(OpenSCManagerW(0, 0, SC_MANAGER_ALL_ACCESS));
    if (hSCM) {
        QString acc = account;
        DWORD dwStartType = startupType == QtServiceController::AutoStartup ? SERVICE_AUTO_START : SERVICE_DEMAND_START;
        DWORD dwServiceType = SERVICE_WIN32_OWN_PROCESS;
        wchar_t *act = 0;
        wchar_t *pwd = 0;
        if (!acc.isEmpty()) {
            // The act string must contain a string of the format "Domain\UserName",
            // so if only a username was specified without a domain, default to the local machine domain.
            if (!acc.contains(QChar('\\'))) {
                acc.prepend(QLatin1String(".\\"));
            }
            if (!acc.endsWith(QLatin1String("\\LocalSystem")))
                act = (wchar_t*)acc.utf16();
        }
        if (!password.isEmpty() && act) {
            pwd = (wchar_t*)password.utf16();
        }

        // The service controller must launch the executable with -s(ervice), without argument it runs as a regular application.
        const QString binaryPath = QLatin1Char('"') + filePath() + QLatin1String("\" -s");

        // Create the service
        const ScHandle hService(CreateServiceW(hSCM, (const wchar_t *)controller.serviceName().utf16(),
                                               (const wchar_t *)controller.serviceName().utf16(),
                                               SERVICE_ALL_ACCESS,
                                               dwServiceType,
                                               dwStartType, SERVICE_ERROR_NORMAL, (const wchar_t *)binaryPath.utf16(),
                                               0, 0, 0,
                                               act, pwd));
        if (hService) {
            result = true;
            if (!serviceDescription.isEmpty()) {
                SERVICE_DESCRIPTIONW sdesc;
                sdesc.lpDescription = (wchar_t *)serviceDescription.utf16();
                ChangeServiceConfig2W(hService, SERVICE_CONFIG_DESCRIPTION, &sdesc);
            }
        }
    } else if (GetLastError() == ERROR_ACCESS_DENIED) {
        fprintf(stderr, "Administrator rights are required to install the service\n");
    }
    return result;
}

QString QtServiceBasePrivate::installationError() const
{
    const ScHandle hSCM(OpenSCManagerW(0, 0, SC_MANAGER_ALL_ACCESS));
    if (!hSCM) {
        if (GetLastError() == ERROR_ACCESS_DENIED)
            return QLatin1String("Administrator rights are required to install or uninstall the service");
        return QLatin1String("The service control manager cannot be opened");
    }
    return QString();
}

QString QtServiceBasePrivate::installationNotice(bool install) const
{
    return QString::fromLatin1(install ? "The Windows service '%1' will be created" : "The Windows service '%1' will be stopped and removed")
        .arg(controller.serviceName());
}

QString QtServiceBasePrivate::filePath() const
{
    // The path may be longer than MAX_PATH: the size is returned when the path has been truncated.
    QVector<wchar_t> path(MAX_PATH);
    DWORD length;
    while ((length = ::GetModuleFileNameW(0, path.data(), path.size())) == DWORD(path.size()))
        path.resize(path.size() * 2);
    return QString::fromWCharArray(path.constData(), length);
}

void QtServiceBasePrivate::sysInit()
{
    sysd = new QtServiceSysPrivate();

    sysd->serviceStatus			    = 0;
    sysd->status.dwServiceType		    = SERVICE_WIN32_OWN_PROCESS;
    sysd->status.dwCurrentState		    = SERVICE_STOPPED;
    sysd->status.dwControlsAccepted         = sysd->serviceFlags(serviceFlags);
    sysd->status.dwWin32ExitCode	    = NO_ERROR;
    sysd->status.dwServiceSpecificExitCode  = 0;
    sysd->status.dwCheckPoint		    = 0;
    sysd->status.dwWaitHint		    = 0;
}

void QtServiceBasePrivate::sysCleanup()
{
    if (sysd) {
        delete sysd;
        sysd = 0;
    }
}

void QtServiceBase::setServiceFlags(QtServiceBase::ServiceFlags flags)
{
    if (d_ptr->serviceFlags == flags)
        return;
    d_ptr->serviceFlags = flags;
    if (d_ptr->sysd)
        d_ptr->sysd->setServiceFlags(flags);
}


