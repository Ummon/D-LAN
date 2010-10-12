#include <priv/GetHashesResult.h>
using namespace PM;

#include <priv/Log.h>

GetHashesResult::GetHashesResult(const Protos::Common::Entry& file, Socket* socket)
   : file(file), socket(socket)
{
}

GetHashesResult::~GetHashesResult()
{
   disconnect(this->socket, SIGNAL(newMessage(quint32, const google::protobuf::Message&)), this, SLOT(newMessage(quint32, const google::protobuf::Message&)));
   this->socket->finished();
}

void GetHashesResult::start()
{
   Protos::Core::GetHashes message;
   message.mutable_file()->CopyFrom(this->file);
   connect(this->socket, SIGNAL(newMessage(quint32, const google::protobuf::Message&)), this, SLOT(newMessage(quint32, const google::protobuf::Message&)));
   socket->send(0x41, message);
}

void GetHashesResult::newMessage(quint32 type, const google::protobuf::Message& message)
{
   switch (type)
   {
   case 0x42:
      {
         const Protos::Core::GetHashesResult& hashesResult = dynamic_cast<const Protos::Core::GetHashesResult&>(message);
         emit result(hashesResult);
      }
      break;

   case 0x43:
      {
         const Protos::Common::Hash& hash = dynamic_cast<const Protos::Common::Hash&>(message);
         emit nextHash(Common::Hash(hash.hash().data()));
      }
      break;

   //case 0x44: // TODO
   }
}
