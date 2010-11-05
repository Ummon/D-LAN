#ifndef COMMON_NETWORK_H
#define COMMON_NETWORK_H

#include <QIODevice>
#include <QDataStream>
#include <QString>

#include <Common/Hash.h>

namespace Common
{
   class notEnoughData {};

   class Network
   {
   public:
      enum CoreMessageType
      {
         // UDP.
         CORE_IM_ALIVE = 0x01,
         CORE_CHUNKS_OWNED = 0x02,

         CORE_CHAT_MESSAGE = 0x11,

         CORE_FIND = 0x21,
         CORE_FIND_RESULT = 0x22,

         // TCP.
         CORE_GET_ENTRIES = 0x31,
         CORE_GET_ENTRIES_RESULT = 0x32,

         CORE_GET_HASHES = 0x41,
         CORE_GET_HASHES_RESULT = 0x42,
         CORE_HASH = 0x43,

         CORE_GET_CHUNK = 0x51,
         CORE_GET_CHUNK_RESULT = 0x52,
      };

      enum GUIMessageType
      {
         GUI_STATE = 0x01,

         GUI_EVENT_CHAT_MESSAGE = 0x11,
         GUI_EVENT_LOG_MESSAGE = 0x12,

         GUI_SETTINGS = 0x21,
         GUI_SEARCH = 0x31,
         GUI_SEARCH_TAG = 0x32,
         GUI_SEARCH_RESULT = 0x33,

         GUI_BROWSE = 0x41,
         GUI_BROWSE_TAG = 0x42,
         GUI_BROWSE_RESULT = 0x43,

         GUI_CANCEL_DOWNLOADS = 0x61,

         GUI_DOWNLOAD = 0x71,

         GUI_CHAT_MESSAGE = 0x81,
      };

      // Template aren't used for 'MessageHeader::type' because it must be 32bits.
      struct MessageHeader
      {
         MessageHeader() : type(0), size(0) {}
         MessageHeader(CoreMessageType type, quint32 size, const Common::Hash senderID) : type(static_cast<quint32>(type)), size(size), senderID(senderID) {}
         MessageHeader(GUIMessageType type, quint32 size, const Common::Hash senderID) : type(static_cast<quint32>(type)), size(size), senderID(senderID) {}

         bool isNull() const { return this->type == 0; }
         void setNull() { this->type = 0; }
         QString toStr() const
         {
            if (this->isNull())
               return QString("MessageHeader : <null>");
            else
               return QString("MessageHeader : type = %1, size = %2, senderID = %3").arg(this->type, 0, 16).arg(this->size).arg(this->senderID.toStr());
         }

         inline CoreMessageType getTypeCore() const { return static_cast<CoreMessageType>(this->type); }
         inline GUIMessageType getTypeGUI() const { return static_cast<GUIMessageType>(this->type); }

         quint32 type;
         quint32 size;
         Common::Hash senderID;
      };

      static QString messToStr(CoreMessageType type);
      static QString messToStr(GUIMessageType type);

      static const int HEADER_SIZE = sizeof(MessageHeader::type) + sizeof(MessageHeader::size) + Hash::HASH_SIZE;

      static MessageHeader readHeader(QIODevice& device, bool skipReadData = true);
      static MessageHeader readHeader(const char* data);

      static void writeHeader(QIODevice& device, const MessageHeader& header);
      static void writeHeader(char* buffer, const MessageHeader& header);

   private:
      inline static void writeHeader(QDataStream& stream, const MessageHeader& header);
   };
}

#endif
