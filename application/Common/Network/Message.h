#ifndef COMMON_MESSAGE_H
#define COMMON_MESSAGE_H

#include <google/protobuf/message.h>

#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Network/MessageHeader.h>

namespace Common
{
   class ReadErrorException {};

   class Message
   {
      Message(const Common::MessageHeader& header, google::protobuf::Message* message);

   public:
      Message(MessageHeader::MessageType type, const QSharedPointer<google::protobuf::Message>& message);
      Message(MessageHeader::MessageType type, google::protobuf::Message* message = nullptr);

      bool isNull() const;

      const MessageHeader& getHeader() const;

      template <typename T = google::protobuf::Message>
      const T& getMessage() const;

      static int writeMessageToBuffer(char* buffer, quint32 bufferSize, const MessageHeader& header, const google::protobuf::Message* message = 0);
      static int writeMessageToDevice(QIODevice* ioDevice, const MessageHeader& header, const google::protobuf::Message* message = 0);

   public:
      /**
        * Read all the message, including header, from the given buffer.
        */
      static Message readMessage(const char* buffer, quint32 bufferSize);

      /**
        * Read the message content from the given device.
        */
      static Message readMessageBodyFromDevice(const MessageHeader& header, QIODevice* ioDevice);

      template <typename T>
      static Message readMessageBody(const MessageHeader& header, T source);

   private:      
      static MessageHeader::MessageType getType(const google::protobuf::Message& message);

      template <typename T>
      static Message readMessageBody(const MessageHeader& header, const char* buffer);
      template <typename T>
      static Message readMessageBody(const MessageHeader& header, ZeroCopyInputStreamQIODevice* stream);

      MessageHeader header;
      QSharedPointer<google::protobuf::Message> message;
   };
}

template <typename T>
const T& Common::Message::getMessage() const
{
   return static_cast<T&>(*this->message.data());
}

/**
  * @exception ReadErrorException
  */
template <typename T>
Message Common::Message::readMessageBody(const MessageHeader& header, T source)
{
   switch (header.getType())
   {
   case MessageHeader::NULL_MESS:                        return readMessageBody<Protos::Common::Null>                (header, source);

   case MessageHeader::CORE_IM_ALIVE:                    return readMessageBody<Protos::Core::IMAlive>               (header, source);
   case MessageHeader::CORE_CHUNKS_OWNED:                return readMessageBody<Protos::Core::ChunksOwned>           (header, source);
   case MessageHeader::CORE_CHAT_MESSAGES:               return readMessageBody<Protos::Common::ChatMessages>        (header, source);
   case MessageHeader::CORE_GET_LAST_CHAT_MESSAGES:      return readMessageBody<Protos::Core::GetLastChatMessages>   (header, source);
   case MessageHeader::CORE_FIND:                        return readMessageBody<Protos::Core::Find>                  (header, source);
   case MessageHeader::CORE_FIND_RESULT:                 return readMessageBody<Protos::Common::FindResult>          (header, source);
   case MessageHeader::CORE_GET_ENTRIES:                 return readMessageBody<Protos::Core::GetEntries>            (header, source);
   case MessageHeader::CORE_GET_ENTRIES_RESULT:          return readMessageBody<Protos::Core::GetEntriesResult>      (header, source);
   case MessageHeader::CORE_GET_HASHES:                  return readMessageBody<Protos::Core::GetHashes>             (header, source);
   case MessageHeader::CORE_GET_HASHES_RESULT:           return readMessageBody<Protos::Core::GetHashesResult>       (header, source);
   case MessageHeader::CORE_HASH:                        return readMessageBody<Protos::Common::Hash>                (header, source);
   case MessageHeader::CORE_GET_CHUNK:                   return readMessageBody<Protos::Core::GetChunk>              (header, source);
   case MessageHeader::CORE_GET_CHUNK_RESULT:            return readMessageBody<Protos::Core::GetChunkResult>        (header, source);

   case MessageHeader::GUI_STATE:                        return readMessageBody<Protos::GUI::State>                  (header, source);
   case MessageHeader::GUI_STATE_RESULT:                 return readMessageBody<Protos::Common::Null>                (header, source);
   case MessageHeader::GUI_EVENT_CHAT_MESSAGES:          return readMessageBody<Protos::Common::ChatMessages>        (header, source);
   case MessageHeader::GUI_EVENT_LOG_MESSAGES:           return readMessageBody<Protos::GUI::EventLogMessages>       (header, source);
   case MessageHeader::GUI_ASK_FOR_AUTHENTICATION:       return readMessageBody<Protos::GUI::AskForAuthentication>   (header, source);
   case MessageHeader::GUI_AUTHENTICATION:               return readMessageBody<Protos::GUI::Authentication>         (header, source);
   case MessageHeader::GUI_AUTHENTICATION_RESULT:        return readMessageBody<Protos::GUI::AuthenticationResult>   (header, source);
   case MessageHeader::GUI_LANGUAGE:                     return readMessageBody<Protos::GUI::Language>               (header, source);
   case MessageHeader::GUI_CHANGE_PASSWORD:              return readMessageBody<Protos::GUI::ChangePassword>         (header, source);
   case MessageHeader::GUI_SETTINGS:                     return readMessageBody<Protos::GUI::CoreSettings>           (header, source);
   case MessageHeader::GUI_SEARCH:                       return readMessageBody<Protos::GUI::Search>                 (header, source);
   case MessageHeader::GUI_SEARCH_TAG:                   return readMessageBody<Protos::GUI::Tag>                    (header, source);
   case MessageHeader::GUI_SEARCH_RESULT:                return readMessageBody<Protos::Common::FindResult>          (header, source);
   case MessageHeader::GUI_BROWSE:                       return readMessageBody<Protos::GUI::Browse>                 (header, source);
   case MessageHeader::GUI_BROWSE_TAG:                   return readMessageBody<Protos::GUI::Tag>                    (header, source);
   case MessageHeader::GUI_BROWSE_RESULT:                return readMessageBody<Protos::GUI::BrowseResult>           (header, source);
   case MessageHeader::GUI_CANCEL_DOWNLOADS:             return readMessageBody<Protos::GUI::CancelDownloads>        (header, source);
   case MessageHeader::GUI_PAUSE_DOWNLOADS:              return readMessageBody<Protos::GUI::PauseDownloads>         (header, source);
   case MessageHeader::GUI_MOVE_DOWNLOADS:               return readMessageBody<Protos::GUI::MoveDownloads>          (header, source);
   case MessageHeader::GUI_DOWNLOAD:                     return readMessageBody<Protos::GUI::Download>               (header, source);
   case MessageHeader::GUI_CHAT_MESSAGE:                 return readMessageBody<Protos::GUI::ChatMessage>            (header, source);
   case MessageHeader::GUI_REFRESH:                      return readMessageBody<Protos::Common::Null>                (header, source);
   case MessageHeader::GUI_REFRESH_NETWORK_INTERFACES:   return readMessageBody<Protos::Common::Null>                (header, source);

   default:                                              return readMessageBody<Protos::Common::Null>                (header, source);
   }
}

/**
  * @exception ReadErrorException
  */
template <typename T>
Message Common::Message::readMessageBody(const MessageHeader& header, const char* buffer)
{
   T* message = new T();
   if (!message->ParseFromArray(buffer, header.getSize()))
   {
      delete message;
      throw ReadErrorException();
   }
   return Message(header, message);
}

/**
  * @exception ReadErrorException
  */
template <typename T>
Message Common::Message::readMessageBody(const MessageHeader& header, ZeroCopyInputStreamQIODevice* stream)
{
   T* message = new T();
   if (!message->ParseFromBoundedZeroCopyStream(stream, header.getSize()))
   {
      delete message;
      throw ReadErrorException();
   }
   return Message(header, message);
}

#endif
