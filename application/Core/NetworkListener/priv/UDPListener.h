#ifndef NETWORKMANAGER_UDPLISTENER_H
#define NETWORKMANAGER_UDPLISTENER_H

#include <QObject>
#include <QSharedPointer>
#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

#include <Common/LogManager/ILogger.h>

#include <Protos/core_protocol.pb.h>

namespace NetworkListener {
    class UDPListener : public QObject {

    Q_OBJECT

    public:
        UDPListener();
        void sendMessage(const QString& mess);

    signals:
        void newChatMessage(const Protos::Core::ChatMessage& message);
        void newFindResult(const Protos::Common::FindResult& result, const quint32& IP);
        void newHaveChunksResult(const Protos::Core::HaveChunksResult& result);

    private:
        QSharedPointer<LogManager::ILogger> logger;
        static const char TTL; ///< Time to live, see the UDP multicast documentation.
        static const int port;
        static QHostAddress multicastIP; ///< A choosen multicast address channel used to send and received messages.
        QUdpSocket* socket;

    private slots:
        void processPendingDatagrams();


   };
}
#endif
