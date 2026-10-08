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
#include <stdio.h>
#include <QTimer>
#include <QVector>

/*!
    \class QtServiceController

    \brief The QtServiceController class allows you to control
    services from separate applications.

    QtServiceController provides a collection of functions that lets
    you run a service controlling its execution, as well as query its
    status.

    In order to run a service, the service must be installed in the
    system's service database: this is done by the service executable
    itself, see the \l {serviceSpecificArguments} {service specific
    arguments} of QtServiceBase. The system will start the service
    depending on the specified StartupType; it can either be started
    during system startup, or when a process starts it manually.

    Once a service is installed, the service can be run and controlled
    manually using the start() and stop() functions.  You can at any
    time query for the service's status using the isInstalled() and
    isRunning() functions. For example:

    \code
    QtServiceController controller("MyService");

    if (controller.isInstalled())
        controller.start()

    if (controller.isRunning())
        QMessageBox::information(this, tr("Service Status"),
                                 tr("The %1 service is started").arg(controller.serviceName()));

    ...

    controller.stop();
    controller.uninstall();
    \endcode

    An instance of the service controller can only control one single
    service. To control several services within one application, you
    must create en equal number of service controllers.

    The QtServiceController destructor neither stops nor uninstalls
    the associated service. To stop a service the stop() function must
    be called explicitly. To uninstall a service, you can use the
    uninstall() function.

    \sa QtServiceBase, QtService
*/

/*!
    \enum QtServiceController::StartupType
    This enum describes when a service should be started.

    \value AutoStartup The service is started during system startup.
    \value ManualStartup The service must be started manually by a process.

    On Linux a service started during system startup is an enabled
    systemd unit.

    \sa QtServiceBase::startupType()
*/


/*!
    Creates a controller object for the service with the given
    \a name.
*/
QtServiceController::QtServiceController(const QString &name)
 : d_ptr(new QtServiceControllerPrivate())
{
    Q_D(QtServiceController);
    d->q_ptr = this;
    d->serviceName = name;
}
/*!
    Destroys the service controller. This neither stops nor uninstalls
    the controlled service.

    To stop a service the stop() function must be called
    explicitly. To uninstall a service, you can use the uninstall()
    function.

    \sa stop(), QtServiceController::uninstall()
*/
QtServiceController::~QtServiceController()
{
    delete d_ptr;
}
/*!
    \fn bool QtServiceController::isInstalled() const

    Returns true if the service is installed; otherwise returns false.

    On Windows it uses the system's service control manager.

    On Linux it checks if the systemd unit of the service exists.
*/

/*!
    \fn bool QtServiceController::isRunning() const

    Returns true if the service is running; otherwise returns false. A
    service must be installed before it can be run using a controller.

    \sa start(), isInstalled()
*/

/*!
    Returns the name of the controlled service.

    \sa QtServiceController()
*/
QString QtServiceController::serviceName() const
{
    Q_D(const QtServiceController);
    return d->serviceName;
}
/*!
    \fn bool QtServiceController::uninstall()

    Uninstalls the service and returns true if successful; otherwise returns false.

    On Windows service is uninstalled using the system's service control manager.

    On Linux the systemd unit of the service is stopped, disabled and removed.
*/

/*!
    \fn bool QtServiceController::start(const QStringList &arguments)

    Starts the installed service passing the given \a arguments to the
    service. A service must be installed before a controller can run it.

    On Linux the \a arguments are ignored: the command line of a
    systemd unit can't be changed when it's started.

    Returns true if the service could be started; otherwise returns
    false.

    \sa stop()
*/

/*!
    \overload

    Starts the installed service without passing any arguments to the service.
*/
bool QtServiceController::start()
{
    return start(QStringList());
}

/*!
    \fn bool QtServiceController::stop()

    Requests the running service to stop. The service will call the
    QtServiceBase::stop() implementation unless the service's state
    is QtServiceBase::CannotBeStopped.  This function does nothing if
    the service is not running.

    Returns true if a running service was successfully stopped;
    otherwise false.

    \sa start(), QtServiceBase::stop(), QtServiceBase::ServiceFlags
*/

QtServiceBase *QtServiceBasePrivate::instance = 0;

QtServiceBasePrivate::QtServiceBasePrivate(const QString &name)
    : startupType(QtServiceController::ManualStartup), serviceFlags(0), controller(name)
{

}

QtServiceBasePrivate::~QtServiceBasePrivate()
{

}

int QtServiceBasePrivate::run(const QStringList &argList)
{
    int argc = argList.size();
    QVector<char *> argv(argc);
    QList<QByteArray> argvData;
    for (int i = 0; i < argc; ++i)
        argvData.append(argList.at(i).toLocal8Bit());
    for (int i = 0; i < argc; ++i)
        argv[i] = argvData[i].data();

    q_ptr->createApplication(argc, argv.data());
    QCoreApplication *app = QCoreApplication::instance();
    if (!app)
        return -1;

#if defined(Q_OS_UNIX)
    installStopSignalHandler();
#endif

    QTimer::singleShot(0, app, [this]() { q_ptr->start(); });
    int res = q_ptr->executeApplication();
    delete app;
    return res;
}


/*!
    \class QtServiceBase

    \brief The QtServiceBase class provides an API for implementing
    Windows services and Unix daemons.

    A Windows service or Unix daemon (a "service"), is a program that
    runs "in the background" independently of whether a user is logged
    in or not. A service is often set up to start when the machine
    boots up, and will typically run continuously as long as the
    machine is on.

    Services are usually non-interactive console applications. User
    interaction, if required, is usually implemented in a separate,
    normal GUI application that communicates with the service through
    an IPC channel, e.g. based on Qt's networking classes.

    Typically, you will create a service by subclassing the QtService
    template class which inherits QtServiceBase and allows you to
    create a service for a particular application type.

    The Windows implementation uses the NT Service Control Manager,
    and the application can be controlled through the system
    administration tools. Services are usually launched using the
    system account, which requires that all DLLs that the service
    executable depends on (i.e. Qt), are located in the same directory
    as the service, or in a system path.

    On Linux a service is a systemd unit, it runs the executable as a
    regular application. The other Unix systems aren't supported.

    You can retrieve the service's description, state, and startup
    type using the serviceDescription(), serviceFlags() and
    startupType() functions respectively. The service's state is
    decribed by the ServiceFlag enum. The mentioned properites can
    also be set using the corresponding set functions. In addition you
    can retrieve the service's name using the serviceName() function.

    The protected functions start() and stop() are called on requests
    from the QtServiceController class.

    You can control any given service using an instance of the
    QtServiceController class which also allows you to control
    services from separate applications. You can reimplement stop()
    to perform additional clean-ups before shutting down, it won't do
    anything unless it is reimplemented.

    QtServiceBase also provides the static instance() function which
    returns a pointer to an application's QtServiceBase instance. In
    addition, a service can report events to the system's event log
    using the logMessage() function. The MessageType enum describes
    the different types of messages a service reports.

    The implementation of a service application's main function
    typically creates an service object derived by subclassing the
    QtService template class. Then the main function will call this
    service's exec() function, and return the result of that call. For
    example:

    \code
        int main(int argc, char **argv)
        {
            MyService service(argc, argv);
            return service.exec();
        }
    \endcode

    When the exec() function is called, it will parse the service
    specific arguments passed in \c argv, perform the required
    actions, and return.

    \target serviceSpecificArguments

    The following arguments are recognized as service specific:

    \table
    \header \i Short \i Long \i Explanation
    \row \i -i \i -install \i Install the service.
    \row \i -u \i -uninstall \i Uninstall the service.
    \row \i -s \i -service \i Start the service.
    \row \i -t \i -terminate \i Stop the service.
    \row \i -v \i -version \i Display version and status information.
    \endtable

    If \e none of the arguments is recognized as service specific, the
    service is executed as a standalone application. This is a blocking
    call, the service will be executed like a normal application. In
    this mode you will not be able to communicate with the service from
    the controller.

    \sa QtService, QtServiceController
*/

/*!
    \enum QtServiceBase::MessageType

    This enum describes the different types of messages a service
    reports to the system log.

    \value Success An operation has succeeded, e.g. the service
           is started.
    \value Error An operation failed, e.g. the service failed to start.
    \value Warning An operation caused a warning that might require user
           interaction.
    \value Information Any type of usually non-critical information.
*/

/*!
    \enum QtServiceBase::ServiceFlag

    This enum describes the different capabilities of a service.

    \value Default The service can be stopped.
    \value CannotBeStopped The service cannot be stopped.
    \value NeedsStopOnShutdown (Windows only) The service will be stopped before the system shuts down. Note that Microsoft recommends this only for services that must absolutely clean up during shutdown, because there is a limited time available for shutdown of services.
*/

/*!
    Creates a service instance called \a name. The \a argc and \a argv
    parameters are parsed after the exec() function has been
    called. Then they are passed to the application's constructor.
    The application type is determined by the QtService subclass.

    The service is neither installed nor started. The name must not
    contain any backslashes or be longer than 255 characters. In
    addition, the name must be unique in the system's service
    database.

    \sa exec(), start()
*/
QtServiceBase::QtServiceBase(int argc, char **argv, const QString &name)
{
    Q_ASSERT(!QtServiceBasePrivate::instance);
    QtServiceBasePrivate::instance = this;

    QString nm(name);
    if (nm.length() > 255) {
	qWarning("QtService: 'name' is longer than 255 characters.");
	nm.truncate(255);
    }
    if (nm.contains('\\')) {
	qWarning("QtService: 'name' contains backslashes '\\'.");
	nm.replace((QChar)'\\', (QChar)'\0');
    }

    d_ptr = new QtServiceBasePrivate(nm);
    d_ptr->q_ptr = this;

    d_ptr->serviceFlags = ServiceFlag::Default;
    d_ptr->sysd = 0;
    for (int i = 0; i < argc; ++i)
        d_ptr->args.append(QString::fromLocal8Bit(argv[i]));
}

/*!
    Destroys the service object. This neither stops nor uninstalls the
    service.

    To stop a service the stop() function must be called
    explicitly. To uninstall a service, you can use the
    QtServiceController::uninstall() function.

    \sa stop(), QtServiceController::uninstall()
*/
QtServiceBase::~QtServiceBase()
{
    delete d_ptr;
    QtServiceBasePrivate::instance = 0;
}

/*!
    Returns the name of the service.

    \sa QtServiceBase(), serviceDescription()
*/
QString QtServiceBase::serviceName() const
{
    return d_ptr->controller.serviceName();
}

/*!
    Returns the description of the service.

    \sa setServiceDescription(), serviceName()
*/
QString QtServiceBase::serviceDescription() const
{
    return d_ptr->serviceDescription;
}

/*!
    Sets the description of the service to the given \a description.

    \sa serviceDescription()
*/
void QtServiceBase::setServiceDescription(const QString &description)
{
    d_ptr->serviceDescription = description;
}

/*!
    Returns the service's startup type.

    \sa QtServiceController::StartupType, setStartupType()
*/
QtServiceController::StartupType QtServiceBase::startupType() const
{
    return d_ptr->startupType;
}

/*!
    Sets the service's startup type to the given \a type.

    \sa QtServiceController::StartupType, startupType()
*/
void QtServiceBase::setStartupType(QtServiceController::StartupType type)
{
    d_ptr->startupType = type;
}

/*!
    Returns the service's state which is decribed using the
    ServiceFlag enum.

    \sa ServiceFlags, setServiceFlags()
*/
QtServiceBase::ServiceFlags QtServiceBase::serviceFlags() const
{
    return d_ptr->serviceFlags;
}

/*!
    \fn void QtServiceBase::setServiceFlags(ServiceFlags flags)

    Sets the service's state to the state described by the given \a
    flags.

    \sa ServiceFlags, serviceFlags()
*/

// Asks a question on the console, the answer is 'no' by default and at the end of the input.
static bool confirm(const QString &question)
{
    printf("%s [y/N] ", question.toLocal8Bit().constData());
    fflush(stdout);
    QByteArray answer;
    for (int c = fgetc(stdin); c != EOF && c != '\n'; c = fgetc(stdin))
        answer.append(char(c));
    answer = answer.trimmed().toLower();
    return answer == "y" || answer == "yes";
}

/*!
    Executes the service.

    When the exec() function is called, it will parse the \l
    {serviceSpecificArguments} {service specific arguments} passed in
    \c argv, perform the required actions, and exit.

    Installing and uninstalling the service ask for a confirmation on
    the console, unless the argument \c --yes is given.

    If none of the arguments is recognized as service specific, exec()
    runs the service as a regular application: it calls the createApplication()
    function, then executeApplication() and finally the start() function,
    and returns when the application exits.

    \sa QtServiceController
*/
int QtServiceBase::exec()
{
    if (d_ptr->args.size() > 1) {
        QString a =  d_ptr->args.at(1);
        // To install or uninstall without confirmation, from an installer for instance.
        const bool assumeYes = d_ptr->args.contains(QLatin1String("--yes"));
        if (a == QLatin1String("-i") || a == QLatin1String("-install")) {
            if (!d_ptr->controller.isInstalled()) {
                const QString error = d_ptr->installationError();
                if (!error.isEmpty()) {
                    fprintf(stderr, "%s\n", error.toLocal8Bit().constData());
                    return -1;
                }
                if (!assumeYes) {
                    if (!confirm(d_ptr->installationNotice(true) + QLatin1String(", would you like to continue?"))) {
                        printf("The service %s has not been installed\n", serviceName().toLatin1().constData());
                        return 0;
                    }
                    d_ptr->startupType = confirm(QLatin1String("Would you like the service to be started at boot?"))
                        ? QtServiceController::AutoStartup : QtServiceController::ManualStartup;
                }
                QStringList parameters = d_ptr->args.mid(2);
                parameters.removeAll(QLatin1String("--yes"));
                const QString account = parameters.value(0);
                const QString password = parameters.value(1);
                if (!d_ptr->install(account, password)) {
                    fprintf(stderr, "The service %s could not be installed\n", serviceName().toLatin1().constData());
                    return -1;
                } else {
                    printf("The service %s has been installed under: %s\n",
                        serviceName().toLatin1().constData(), d_ptr->filePath().toLatin1().constData());
                }
            } else {
                fprintf(stderr, "The service %s is already installed\n", serviceName().toLatin1().constData());
            }
            return 0;
        } else if (a == QLatin1String("-u") || a == QLatin1String("-uninstall")) {
            if (d_ptr->controller.isInstalled()) {
                const QString error = d_ptr->installationError();
                if (!error.isEmpty()) {
                    fprintf(stderr, "%s\n", error.toLocal8Bit().constData());
                    return -1;
                }
                if (!assumeYes && !confirm(d_ptr->installationNotice(false) + QLatin1String(", would you like to continue?"))) {
                    printf("The service %s has not been uninstalled\n", serviceName().toLatin1().constData());
                    return 0;
                }
                if (!d_ptr->controller.uninstall()) {
                    fprintf(stderr, "The service %s could not be uninstalled\n", serviceName().toLatin1().constData());
                    return -1;
                } else {
                    printf("The service %s has been uninstalled.\n",
                        serviceName().toLatin1().constData());
                }
            } else {
                fprintf(stderr, "The service %s is not installed\n", serviceName().toLatin1().constData());
            }
            return 0;
        } else if (a == QLatin1String("-v") || a == QLatin1String("-version")) {
            printf("The service\n"
                "\t%s\n\t%s\n\n", serviceName().toLatin1().constData(), d_ptr->args.at(0).toLatin1().constData());
            printf("is %s", (d_ptr->controller.isInstalled() ? "installed" : "not installed"));
            printf(" and %s\n\n", (d_ptr->controller.isRunning() ? "running" : "not running"));
            return 0;
        } else if (a == QLatin1String("-s") || a == QLatin1String("-service")) {
            d_ptr->args.removeAt(1);
            if (!d_ptr->start()) {
                fprintf(stderr, "The service %s could not start\n", serviceName().toLatin1().constData());
                return -4;
            }
            return 0;
        } else if (a == QLatin1String("-t") || a == QLatin1String("-terminate")) {
            if (!d_ptr->controller.stop())
                qErrnoWarning("The service could not be stopped.");
            return 0;
        } else  if (a == QLatin1String("-h") || a == QLatin1String("-help")) {
            printf("\n%s -[i|u|s|t|v|h]\n"
                   "\t-i(nstall) [account] [password]\t: Install the service, optionally using given account and password\n"
                   "\t-u(ninstall)\t: Uninstall the service.\n"
                   "\t-s(ervice)\t: Start the service.\n"
                   "\t-t(erminate)\t: Stop the service.\n"
                   "\t-v(ersion)\t: Print version and status information.\n"
                   "\t-h(elp)   \t: Show this help\n"
                   "\tNo arguments\t: Run as a regular application.\n",
                   d_ptr->args.at(0).toLatin1().constData());
            return 0;
        }
    }
    int ec = d_ptr->run(d_ptr->args);
    if (ec == -1)
        qErrnoWarning("The service could not be executed.");
    return ec;
}

/*!
    Returns true if the service is running as a system service (Windows);
    returns false if it is running as a regular application, which is
    always the case on Unix: systemd runs the service like a regular
    application.
*/
bool QtServiceBase::isRunningAsService() const
{
    return d_ptr->sysd != 0;
}

/*!
    \fn void QtServiceBase::logMessage(const QString &message, MessageType type,
            int id, uint category, const QByteArray &data)

    Reports a message of the given \a type with the given \a message
    to the local system event log.  The message identifier \a id and
    the message \a category are user defined values. The \a data
    parameter can contain arbitrary binary data.

    Message strings for \a id and \a category must be provided by a
    message file, which must be registered in the system registry.
    Refer to the MSDN for more information about how to do this on
    Windows.

    \sa MessageType
*/

/*!
    Returns a pointer to the current application's QtServiceBase
    instance.
*/
QtServiceBase *QtServiceBase::instance()
{
    return QtServiceBasePrivate::instance;
}

/*!
    \fn void QtServiceBase::start()

    This function must be implemented in QtServiceBase subclasses in
    order to perform the service's work. Usually you create some main
    object on the heap which is the heart of your service.

    The function is only called when no service specific arguments
    were passed to the service constructor, and is called by exec()
    after it has called the executeApplication() function.

    Note that you \e don't need to create an application object or
    call its exec() function explicitly.

    \sa exec(), stop(), QtServiceController::start()
*/

/*!
    Reimplement this function to perform additional cleanups before
    shutting down (for example deleting a main object if it was
    created in the start() function).

    This function is called in reply to controller requests. The
    default implementation does nothing.

    \sa start(), QtServiceController::stop()
*/
void QtServiceBase::stop()
{
}

/*!
    \fn void QtServiceBase::createApplication(int &argc, char **argv)

    Creates the application object using the \a argc and \a argv
    parameters.

    This function is only called when no \l
    {serviceSpecificArguments}{service specific arguments} were
    passed to the service constructor, and is called by exec() before
    it calls the executeApplication() and start() functions.

    The createApplication() function is implemented in QtService, but
    you might want to reimplement it, for example, if the chosen
    application type's constructor needs additional arguments.

    \sa exec(), QtService
*/

/*!
    \fn int QtServiceBase::executeApplication()

    Executes the application previously created with the
    createApplication() function.

    This function is only called when no \l
    {serviceSpecificArguments}{service specific arguments} were
    passed to the service constructor, and is called by exec() after
    it has called the createApplication() function and before start() function.

    This function is implemented in QtService.

    \sa exec(), createApplication()
*/

/*!
    \class QtService

    \brief The QtService is a convenient template class that allows
    you to create a service for a particular application type.

    A Windows service or Unix daemon (a "service"), is a program that
    runs "in the background" independently of whether a user is logged
    in or not. A service is often set up to start when the machine
    boots up, and will typically run continuously as long as the
    machine is on.

    Services are usually non-interactive console applications. User
    interaction, if required, is usually implemented in a separate,
    normal GUI application that communicates with the service through
    an IPC channel, e.g. based on Qt's networking classes.

    The QtService class functionality is inherited from QtServiceBase,
    but in addition the QtService class binds an instance of
    QtServiceBase with an application type.

    Typically, you will create a service by subclassing the QtService
    template class. For example:

    \code
    class MyService : public QtService<QApplication>
    {
    public:
        MyService(int argc, char **argv);
        ~MyService();

    protected:
        void start();
        void stop();
    };
    \endcode

    The application type can be QCoreApplication for services without
    GUI, QApplication for services with GUI or you can use your own
    custom application type.

    You must reimplement the QtServiceBase::start() function to
    perform the service's work. Usually you create some main object on
    the heap which is the heart of your service.

    In addition, you might want to reimplement QtServiceBase::stop()
    to perform additional clean-ups before shutting down. You can
    control any given service using an instance of the
    QtServiceController class which also allows you to control
    services from separate applications.

    Your custom service is typically instantiated in the application's
    main function. Then the main function will call your service's
    exec() function, and return the result of that call. For example:

    \code
        int main(int argc, char **argv)
        {
            MyService service(argc, argv);
            return service.exec();
        }
    \endcode

    When the exec() function is called, it will parse the \l
    {serviceSpecificArguments} {service specific arguments} passed in
    \c argv, perform the required actions, and exit.

    If none of the arguments is recognized as service specific, exec()
    runs the service as a regular application: it calls the createApplication()
    function, then executeApplication() and finally the start() function,
    and returns when the application exits.

    \sa QtServiceBase, QtServiceController
*/

/*!
    \fn QtService::QtService(int argc, char **argv, const QString &name)

    Constructs a QtService object called \a name. The \a argc and \a
    argv parameters are parsed after the exec() function has been
    called. Then they are passed to the application's constructor.

    There can only be one QtService object in a process.

    \sa QtServiceBase()
*/

/*!
    \fn QtService::~QtService()

    Destroys the service object.
*/

/*!
    \fn Application *QtService::application() const

    Returns a pointer to the application object.
*/

/*!
    \fn void QtService::createApplication(int &argc, char **argv)

    Creates application object of type Application passing \a argc and
    \a argv to its constructor.

    \reimp

*/

/*!
    \fn int QtService::executeApplication()

    \reimp
*/



