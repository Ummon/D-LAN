/****************************************************************************
** 
** Copyright (c) 2009 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
** 
** This file is part of a Qt Solutions component.
**
** Commercial Usage  
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Solutions Commercial License Agreement provided
** with the Software or, alternatively, in accordance with the terms
** contained in a written agreement between you and Nokia.
** 
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
** 
** In addition, as a special exception, Nokia gives you certain
** additional rights. These rights are described in the Nokia Qt LGPL
** Exception version 1.1, included in the file LGPL_EXCEPTION.txt in this
** package.
** 
** GNU General Public License Usage 
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
** 
** Please note Third Party Software included with Qt Solutions may impose
** additional restrictions and it is the user's responsibility to ensure
** that they have met the licensing requirements of the GPL, LGPL, or Qt
** Solutions Commercial license and the relevant license of the Third
** Party Software they are using.
** 
** If you are unsure which license is appropriate for your use, please
** contact Nokia at qt-info@nokia.com.
** 
****************************************************************************/

#include <QtCore/QStringList>
#include <QtCore/QDir>
#include <QtCore/QSettings>
#include "qtservice.h"

int processArgs(int argc, char **argv)
{
    if (argc > 2) {
        QString arg1(argv[1]);
	if (arg1 == QLatin1String("-i") ||
            arg1 == QLatin1String("-install")) {
	    if (argc > 2) {
                QString account;
                QString password;
		QString path(argv[2]);
                if (argc > 3)
                    account = argv[3];
                if (argc > 4)
                    password = argv[4];
                printf("The service %s installed.\n",
                       (QtServiceController::install(path, account, password) ? "was" : "was not"));
                return 0;
            }
        } else {
            QString serviceName(argv[1]);
            QtServiceController controller(serviceName);
            QString option(argv[2]);
            if (option == QLatin1String("-u") ||
                option == QLatin1String("-uninstall")) {
                printf("The service \"%s\" %s uninstalled.\n",
                            controller.serviceName().toLatin1().constData(),
                            (controller.uninstall() ? "was" : "was not"));
                return 0;
            } else if (option == QLatin1String("-s") ||
                       option == QLatin1String("-start")) {
                QStringList args;
                for (int i = 3; i < argc; ++i)
                    args.append(QString::fromLocal8Bit(argv[i]));
                printf("The service \"%s\" %s started.\n",
                       controller.serviceName().toLatin1().constData(),
                            (controller.start(args) ? "was" : "was not"));
                return 0;
            } else if (option == QLatin1String("-t") ||
                       option == QLatin1String("-terminate")) {
                printf("The service \"%s\" %s stopped.\n",
                       controller.serviceName().toLatin1().constData(),
                       (controller.stop() ? "was" : "was not"));
                return 0;
            } else if (option == QLatin1String("-p") ||
                    option == QLatin1String("-pause")) {
                printf("The service \"%s\" %s paused.\n",
                       controller.serviceName().toLatin1().constData(),
                       (controller.pause() ? "was" : "was not"));
                return 0;
            } else if (option == QLatin1String("-r") ||
                       option == QLatin1String("-resume")) {
                printf("The service \"%s\" %s resumed.\n",
                       controller.serviceName().toLatin1().constData(),
                       (controller.resume() ? "was" : "was not"));
                return 0;
            } else if (option == QLatin1String("-c") ||
                       option == QLatin1String("-command")) {
                if (argc > 3) {
                    QString codestr(argv[3]);
                    int code = codestr.toInt();
                    printf("The command %s sent to the service \"%s\".\n",
                           (controller.sendCommand(code) ? "was" : "was not"),
                           controller.serviceName().toLatin1().constData());
                    return 0;
                }
            } else if (option == QLatin1String("-v") ||
                    option == QLatin1String("-version")) {
                bool installed = controller.isInstalled();
                printf("The service\n"
                        "\t\"%s\"\n\n", controller.serviceName().toLatin1().constData());
                printf("is %s", (installed ? "installed" : "not installed"));
                printf(" and %s\n\n", (controller.isRunning() ? "running" : "not running"));
                if (installed) {
                    printf("path: %s\n", controller.serviceFilePath().toLatin1().data());
                    printf("description: %s\n", controller.serviceDescription().toLatin1().data());
                    printf("startup: %s\n", controller.startupType() == QtServiceController::AutoStartup ? "Auto" : "Manual");
                }
                return 0;
            }
        }
    }
    printf("controller [-i PATH | SERVICE_NAME [-v | -u | -s | -t | -p | -r | -c CODE] | -h] [-w]\n\n"
            "\t-i(nstall) PATH\t: Install the service\n"
            "\t-v(ersion)\t: Print status of the service\n"
            "\t-u(ninstall)\t: Uninstall the service\n"
            "\t-s(tart)\t: Start the service\n"
            "\t-t(erminate)\t: Stop the service\n"
            "\t-p(ause)\t: Pause the service\n"
            "\t-r(esume)\t: Resume the service\n"
            "\t-c(ommand) CODE\t: Send a command to the service\n"
            "\t-h(elp)\t\t: Print this help info\n"
            "\t-w(ait)\t\t: Wait for keypress when done\n");
    return 0;
}


int main(int argc, char **argv)
{
#if !defined(Q_WS_WIN)
    // QtService stores service settings in SystemScope, which normally require root privileges.
    // To allow testing this example as non-root, we change the directory of the SystemScope settings file.
    QSettings::setPath(QSettings::NativeFormat, QSettings::SystemScope, QDir::tempPath());
    qWarning("(Example uses dummy settings file: %s/QtSoftware.conf)", QDir::tempPath().toLatin1().constData());
#endif

    int result = processArgs(argc, argv);

    if (QString::fromLocal8Bit(argv[argc-1]) == QLatin1String("-w") ||
        QString::fromLocal8Bit(argv[argc-1]) == QLatin1String("-wait")) {
        printf("\nPress Enter to continue...");
        QFile input;
        input.open(stdin, QIODevice::ReadOnly);
        input.readLine();
        printf("\n");
    }

    return result;
}
