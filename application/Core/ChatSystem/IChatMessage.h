#ifndef CHATSYSTEM_ICHATMESSAGE_H
#define CHATSYSTEM_ICHATMESSAGE_H

namespace CS
{
   class IChatMessage
   {
   public:
      virtual ~IChatMessage() {}

      virtual QString getMessage() const = 0;
   };
}

#endif
