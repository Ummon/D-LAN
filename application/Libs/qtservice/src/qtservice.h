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

#ifndef QTSERVICE_H
#define QTSERVICE_H

#include <QtCore/QCoreApplication>

#if defined(Q_WS_WIN)
#  if !defined(QT_QTSERVICE_EXPORT) && !defined(QT_QTSERVICE_IMPORT)
#    define QT_QTSERVICE_EXPORT
#  elif defined(QT_QTSERVICE_IMPORT)
#    if defined(QT_QTSERVICE_EXPORT)
#      undef QT_QTSERVICE_EXPORT
#    endif
#    define QT_QTSERVICE_EXPORT __declspec(dllimport)
#  elif defined(QT_QTSERVICE_EXPORT)
#    undef QT_QTSERVICE_EXPORT
#    define QT_QTSERVICE_EXPORT __declspec(dllexport)
#  endif
#else
#  define QT_QTSERVICE_EXPORT
#endif

class QStringList;
class QtServiceControllerPrivate;

class QT_QTSERVICE_EXPORT QtServiceController
{
    Q_DECLARE_PRIVATE(QtServiceController)
public:
    enum StartupType
    {
	    AutoStartup = 0, ManualStartup
    };

    QtServiceController(const QString &name);
    virtual ~QtServiceController();

    bool isInstalled() const;
    bool isRunning() const;

    QString serviceName() const;
    QString serviceDescription() const;
    StartupType startupType() const;
    QString serviceFilePath() const;

    static bool install(const QString &serviceFilePath, const QString &account = QString(),
                const QString &password = QString());
    bool uninstall();

    bool start(const QStringList &arguments);
    bool start();
    bool stop();
    bool pause();
    bool resume();
    bool sendCommand(int code);

private:
    QtServiceControllerPrivate *d_ptr;
};

class QtServiceBasePrivate;

class QT_QTSERVICE_EXPORT QtServiceBase
{
    Q_DECLARE_PRIVATE(QtServiceBase)
public:

    enum MessageType
    {
	Success = 0, Error, Warning, Information
    };

    enum ServiceFlag
    {
        Default = 0x00,
        CanBeSuspended = 0x01,
        CannotBeStopped = 0x02
    };

    Q_DECLARE_FLAGS(ServiceFlags, ServiceFlag)

    QtServiceBase(int argc, char **argv, const QString &name);
    virtual ~QtServiceBase();

    QString serviceName() const;

    QString serviceDescription() const;
    void setServiceDescription(const QString &description);

    QtServiceController::StartupType startupType() const;
    void setStartupType(QtServiceController::StartupType startupType);

    ServiceFlags serviceFlags() const;
    void setServiceFlags(ServiceFlags flags);

    int exec();

    void logMessage(const QString &message, MessageType type = Success,
                int id = 0, uint category = 0, const QByteArray &data = QByteArray());

    static QtServiceBase *instance();

protected:

    virtual void start() = 0;
    virtual void stop();
    virtual void pause();
    virtual void resume();
    virtual void processCommand(int code);

    virtual void createApplication(int &argc, char **argv) = 0;

    virtual int executeApplication() = 0;

private:

    friend class QtServiceSysPrivate;
    QtServiceBasePrivate *d_ptr;
};

template <typename Application>
class QtService : public QtServiceBase
{
public:
    QtService(int argc, char **argv, const QString &name)
        : QtServiceBase(argc, argv, name), app(0)
    {  }
    ~QtService()
    {
    }

protected:
    Application *application() const
    { return app; }

    virtual void createApplication(int &argc, char **argv)
    {
        app = new Application(argc, argv);
        QCoreApplication *a = app;
        Q_UNUSED(a);
    }

    virtual int executeApplication()
    { return Application::exec(); }

private:
    Application *app;
};

Q_DECLARE_OPERATORS_FOR_FLAGS(QtServiceBase::ServiceFlags)

#endif // QTSERVICE_H
