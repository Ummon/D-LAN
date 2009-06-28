#ifndef CHAT_H
#define CHAT_H

#include <QObject>

#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

/**
  * An instance of this class will listen for UDP datagram on a certain port.
  * When a message is received it will print it.
  * Broadcast nessage can be sended eith 'sendMessage'.
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
    static const int port = 34326;
};

#endif // CHAT_H
