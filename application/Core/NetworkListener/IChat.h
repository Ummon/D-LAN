#ifndef NETWORKMANAGER_ICHAT_H
#define NETWORKMANAGER_ICHAT_H

#include <QObject>
#include <QString>

#include <Protos/core_protocol.pb.h>

namespace NetworkListener
{
   class IChat : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IChat() {}

      //virtual void send(const QString& message) = 0;

   signals:
      void newMessage(const Protos::Core::ChatMessage& message);
   };
}
#endif
