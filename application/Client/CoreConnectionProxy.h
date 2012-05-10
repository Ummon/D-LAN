#ifndef CLIENT_CORECONNECTIONPROXY_H
#define CLIENT_CORECONNECTIONPROXY_H

#include <QObject>
#include <QSharedPointer>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace Client
{
   class CoreConnectionProxy : public QObject
   {
      Q_OBJECT
   public:
      CoreConnectionProxy();

   public slots:
      void tryConnecting();
      void sendChatMessage(const QString& message);

   signals:
      void connected();

   private:
      QSharedPointer<RCC::ICoreConnection> coreConnection;
   };
}

#endif
