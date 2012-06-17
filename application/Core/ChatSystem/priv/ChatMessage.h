#ifndef CHATSYSTEM_CHATMESSAGE_H
#define CHATSYSTEM_CHATMESSAGE_H

#include <QString>
#include <QDateTime>

#include <Libs/MersenneTwister.h>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace CS
{
   class ChatMessage
   {
   public:
      ChatMessage(const QString& message, const Common::Hash& ownerID, const QString& ownerNick);
      ChatMessage(const Protos::Common::ChatMessage& chatMessage);

      quint64 getID() const;
      QDateTime getTime() const;

      void fillProtoChatMessage(Protos::Common::ChatMessage& protoChatMessage) const;

   private:
      static MTRand mtrand;

      quint64 ID;
      QString message;
      Common::Hash ownerID;
      QDateTime time; // UTC.
      QString ownerNick;
   };
}

#endif
