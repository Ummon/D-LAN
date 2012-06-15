#include <Common/Network/Message.h>
using namespace Common;

#include <QIODevice>

Message::Message(const MessageHeader& header, google::protobuf::Message* message) :
   header(header), message(message)
{
}

//Message::Message(const MessageHeader::MessageType type, const QSharedPointer<google::protobuf::Message>& message) :
//   header(type), message(message)
//{
//}

///**
//  * The object takes the control of 'message' and will destroy it when no more used.
//  */
//Message::Message(const MessageHeader::MessageType type, google::protobuf::Message* message) :
//   header(type), message(message)
//{
//}

bool Message::isNull() const
{
   return this->message.isNull();
}

const MessageHeader& Message::getHeader() const
{
   return this->header;
}

//void Message::setHeaderSize(quint32 size)
//{
//   this->header.setSize(size);
//}

//void Message::setHeaderPeerID(const Hash& ID)
//{
//   this->header.setSenderID(ID);
//}

int Message::writeMessageToBuffer(char* buffer, quint32 bufferSize, const MessageHeader& header, const google::protobuf::Message* message)
{
   if (MessageHeader::HEADER_SIZE + header.getSize() > bufferSize)
      return 0;

   MessageHeader::writeHeader(buffer, header);

   if (message)
      message->SerializeToArray(buffer + MessageHeader::HEADER_SIZE, bufferSize - MessageHeader::HEADER_SIZE);

   return MessageHeader::HEADER_SIZE + header.getSize();
}

int Message::writeMessageToDevice(QIODevice* ioDevice, const MessageHeader& header, const google::protobuf::Message* message)
{
   MessageHeader::writeHeader(*ioDevice, header);

   if (message)
   {
      ZeroCopyOutputStreamQIODevice outputStream(ioDevice);
      if (!message->SerializeToZeroCopyStream(&outputStream))
         return 0;
   }

   return MessageHeader::HEADER_SIZE + header.getSize();
}

/**
  * @exception ReadErrorException
  */
Message Message::readMessage(const char* buffer, quint32 bufferSize)
{
   MessageHeader header = MessageHeader::readHeader(buffer);

   if (header.getSize() > bufferSize - MessageHeader::HEADER_SIZE)
      throw ReadErrorException();

   const char* bufferBody = buffer + MessageHeader::HEADER_SIZE;

   return readMessageBody(header, bufferBody);
}

Message Message::readMessageBodyFromDevice(const MessageHeader& header, QIODevice* ioDevice)
{
   ZeroCopyInputStreamQIODevice inputStream(ioDevice);
   return readMessageBody(header, &inputStream);
}
