#ifndef CHAT_H
#define CHAT_H

#include <QObject>

#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

/**
  * An instance of this class will listen for UDP datagram on a certain port and IP (class D).
  * see : http://tldp.org/HOWTO/Multicast-HOWTO-2.html#ss2.1
  * When a message is received it will print it.
  * Multicast nessages can be sended with 'sendMessage'.
  */
class Chat : public QObject
{
   Q_OBJECT
   
public:
    Chat();
    virtual ~Chat();
    
    /**
      * Send a broadcast message.
      */
    void sendMessage(const QString& mess);

private slots:
    void processPendingDatagrams();
    
private:
    QUdpSocket* socket;
    
    static const char TTL;
    static const int port;
    static QHostAddress multicastIP;
};

#endif // CHAT_H
