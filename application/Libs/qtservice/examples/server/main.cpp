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

#include <QtCore/QCoreApplication>
#include <QtNetwork/QTcpServer>
#include <QtNetwork/QTcpSocket>
#include <QtCore/QTextStream>
#include <QtCore/QDateTime>
#include <QtCore/QStringList>
#include <QtCore/QDir>
#include <QtCore/QSettings>

#include "qtservice.h"

// HttpDaemon is the the class that implements the simple HTTP server.
class HttpDaemon : public QTcpServer
{
    Q_OBJECT
public:
    HttpDaemon(quint16 port, QObject* parent = 0)
        : QTcpServer(parent), disabled(false)
    {
        listen(QHostAddress::Any, port);
    }

    void incomingConnection(int socket)
    {
        if (disabled)
            return;

        // When a new client connects, the server constructs a QTcpSocket and all
        // communication with the client is done over this QTcpSocket. QTcpSocket
        // works asynchronously, this means that all the communication is done
        // in the two slots readClient() and discardClient().
        QTcpSocket* s = new QTcpSocket(this);
        connect(s, SIGNAL(readyRead()), this, SLOT(readClient()));
        connect(s, SIGNAL(disconnected()), this, SLOT(discardClient()));
        s->setSocketDescriptor(socket);

        QtServiceBase::instance()->logMessage("New Connection");
    }

    void pause()
    {
        disabled = true;
    }

    void resume()
    {
        disabled = false;
    }

private slots:
    void readClient()
    {
        if (disabled)
            return;

        // This slot is called when the client sent data to the server. The
        // server looks if it was a get request and sends a very simple HTML
        // document back.
        QTcpSocket* socket = (QTcpSocket*)sender();
        if (socket->canReadLine()) {
            QStringList tokens = QString(socket->readLine()).split(QRegExp("[ \r\n][ \r\n]*"));
            if (tokens[0] == "GET") {
                QTextStream os(socket);
                os.setAutoDetectUnicode(true);
                os << "HTTP/1.0 200 Ok\r\n"
                    "Content-Type: text/html; charset=\"utf-8\"\r\n"
                    "\r\n"
                    "<h1>Nothing to see here</h1>\n"
                    << QDateTime::currentDateTime().toString() << "\n";
                socket->close();

                QtServiceBase::instance()->logMessage("Wrote to client");

                if (socket->state() == QTcpSocket::UnconnectedState) {
                    delete socket;
                    QtServiceBase::instance()->logMessage("Connection closed");
                }
            }
        }
    }
    void discardClient()
    {
        QTcpSocket* socket = (QTcpSocket*)sender();
        socket->deleteLater();

        QtServiceBase::instance()->logMessage("Connection closed");
    }

private:
    bool disabled;
};

class HttpService : public QtService<QCoreApplication>
{
public:
    HttpService(int argc, char **argv)
	: QtService<QCoreApplication>(argc, argv, "Qt HTTP Daemon")
    {
        setServiceDescription("A dummy HTTP service implemented with Qt");
        setServiceFlags(QtServiceBase::CanBeSuspended);
    }
    ~HttpService()
    {
       QtServiceBase::instance()->logMessage("Plomp");
    }

protected:
    void start()
    {
        QCoreApplication *app = application();

        quint16 port = (app->argc() > 1) ?
                QString::fromLocal8Bit(app->argv()[1]).toUShort() : 8080;
        daemon = new HttpDaemon(port, app);

        if (!daemon->isListening()) {
            logMessage(QString("Failed to bind to port %1").arg(daemon->serverPort()), QtServiceBase::Error);
            app->quit();
        }
    }

    void pause()
    {
	daemon->pause();
    }

    void resume()
    {
	daemon->resume();
    }

private:
    HttpDaemon *daemon;
};

#include "main.moc"

int main(int argc, char **argv)
{
#if !defined(Q_WS_WIN)
    // QtService stores service settings in SystemScope, which normally require root privileges.
    // To allow testing this example as non-root, we change the directory of the SystemScope settings file.
    QSettings::setPath(QSettings::NativeFormat, QSettings::SystemScope, QDir::tempPath());
    qWarning("(Example uses dummy settings file: %s/QtSoftware.conf)", QDir::tempPath().toLatin1().constData());
#endif
    HttpService service(argc, argv);
    return service.exec();
}
