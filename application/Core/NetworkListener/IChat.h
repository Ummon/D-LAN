#ifndef NETWORKLISTENER_ICHAT_H
#define NETWORKLISTENER_ICHAT_H

#include <QObject>
#include <QString>

#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

namespace NL
{
   class IChat : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IChat() {}
      virtual void send(const QString& message) = 0;

   signals:
      void newMessage(const Common::Hash& peerID, const Protos::Core::ChatMessage& message);
   };
}
#endif
