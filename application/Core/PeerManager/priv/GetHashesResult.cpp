#include <priv/GetHashesResult.h>
using namespace PM;

#include <priv/Log.h>

GetHashesResult::GetHashesResult(const Protos::Common::Entry& file, QSharedPointer<Socket> socket)
   : file(file), socket(socket)
{
}

GetHashesResult::~GetHashesResult()
{
   disconnect(this->socket.data(), SIGNAL(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)));
   this->socket->finished();
}

void GetHashesResult::start()
{
   Protos::Core::GetHashes message;
   message.mutable_file()->CopyFrom(this->file);
   connect(this->socket.data(), SIGNAL(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)));
   socket->send(Common::Network::CORE_GET_HASHES, message);
}

void GetHashesResult::newMessage(Common::Network::CoreMessageType type, const google::protobuf::Message& message)
{
   switch (type)
   {
   case Common::Network::CORE_GET_HASHES_RESULT:
      {
         const Protos::Core::GetHashesResult& hashesResult = dynamic_cast<const Protos::Core::GetHashesResult&>(message);
         emit result(hashesResult);
      }
      break;

   case Common::Network::CORE_HASH:
      {
         const Protos::Common::Hash& hash = dynamic_cast<const Protos::Common::Hash&>(message);
         emit nextHash(Common::Hash(hash.hash().data()));
      }
      break;

   default:;
   }
}
