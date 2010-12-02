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
         CORE_NULL = 0x00,

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
         GUI_NULL = 0x00,

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
         GUI_MOVE_DOWNLOADS = 0x71,

         GUI_DOWNLOAD = 0x81,

         GUI_CHAT_MESSAGE = 0x91,
      };

      static QString messToStr(CoreMessageType type);
      static QString messToStr(GUIMessageType type);

      // T must be a 32bits type.
      template <typename T>
      struct MessageHeader
      {
         MessageHeader() : type(T(0)), size(0) {}
         MessageHeader(T type, quint32 size, const Common::Hash senderID) : type(type), size(size), senderID(senderID) {}

         bool isNull() const { return this->type == T(0); }
         void setNull() { this->type = T(0); }
         QString toStr() const
         {
            if (this->isNull())
               return QString("MessageHeader : <null>");
            else
               return QString("MessageHeader : type = %1, size = %2, senderID = %3").arg(messToStr(this->type)).arg(this->size).arg(this->senderID.toStr());
         }

         T type;
         quint32 size;
         Common::Hash senderID;
      };

      static const int HEADER_SIZE = sizeof(MessageHeader<quint32>::type) + sizeof(MessageHeader<quint32>::size) + Hash::HASH_SIZE;

      template <typename T>
      static MessageHeader<T> readHeader(QIODevice& device, bool skipReadData = true);

      template <typename T>
      static MessageHeader<T> readHeader(const char* data);

      template <typename T>
      static void writeHeader(QIODevice& device, const MessageHeader<T>& header);

      template <typename T>
      static void writeHeader(char* buffer, const MessageHeader<T>& header);

   private:
      template <typename T>
      inline static void writeHeader(QDataStream& stream, const MessageHeader<T>& header);
   };
}

/***** Definition *****/
using namespace Common;

template <typename T>
Network::MessageHeader<T> Network::readHeader(QIODevice& device, bool skipReadData)
{
   if (device.bytesAvailable() < HEADER_SIZE)
      throw notEnoughData();

   char data[HEADER_SIZE];

   if (skipReadData)
      device.read(data, HEADER_SIZE);
   else
      device.peek(data, HEADER_SIZE);

   return Network::readHeader<T>(data);
}

/**
  * @remarks The buffer size must be at least the header size (28 bytes).
  */
template <typename T>
Network::MessageHeader<T> Network::readHeader(const char* data)
{
   MessageHeader<T> header;

   QByteArray array = QByteArray::fromRawData(data, HEADER_SIZE);
   QDataStream stream(&array, QIODevice::ReadOnly);

   quint32 type;
   stream >> type;
   header.type = T(type);

   stream >> header.size;
   stream >> header.senderID;

   return header;
}

template <typename T>
void Network::writeHeader(QIODevice& device, const MessageHeader<T>& header)
{
   QDataStream stream(&device);
   Network::writeHeader(stream, header);
}

template <typename T>
void Network::writeHeader(char* buffer, const MessageHeader<T>& header)
{
   // We can't use 'fromRawData' because QByteArray can't modify the given buffer.
   // QByteArray array = QByteArray::fromRawData(buffer, HEADER_SIZE);

   QByteArray array(HEADER_SIZE, 0);
   QDataStream stream(&array, QIODevice::WriteOnly);
   Network::writeHeader(stream, header);
   memcpy(buffer, array.data(), HEADER_SIZE);
}

template <typename T>
inline void Network::writeHeader(QDataStream& stream, const MessageHeader<T>& header)
{
   stream << header.type;
   stream << header.size;
   stream << header.senderID;
}

#endif
