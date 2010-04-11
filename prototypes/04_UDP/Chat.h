#ifndef CHAT_H
#define CHAT_H

#include <QObject>

#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

class Chat : public QObject
{
   Q_OBJECT
   
public:
    Chat();
    virtual ~Chat();
    void sendMessage(const QString& mess);

private slots:
    void processPendingDatagrams();
    
private:
    QUdpSocket* socket;
    
    static const char TTL; ///< Time to live, see the UDP multicast documentation.
    static const int port;
    static QHostAddress multicastIP; ///< A choosen multicast address channel used to send and received messages.
};

#endif
