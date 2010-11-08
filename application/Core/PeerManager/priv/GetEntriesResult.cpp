#include <priv/GetEntriesResult.h>
using namespace PM;

#include <priv/Log.h>

GetEntriesResult::GetEntriesResult(const Protos::Core::GetEntries& dir, QSharedPointer<Socket> socket)
   : dir(dir), socket(socket)
{
}

GetEntriesResult::~GetEntriesResult()
{
}

void GetEntriesResult::start()
{
   connect(this->socket.data(), SIGNAL(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), Qt::DirectConnection);
   socket->send(Common::Network::CORE_GET_ENTRIES, this->dir);
}

void GetEntriesResult::newMessage(Common::Network::CoreMessageType type, const google::protobuf::Message& message)
{
   if (type != Common::Network::CORE_GET_ENTRIES_RESULT)
      return;

   disconnect(this->socket.data(), SIGNAL(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)));

   const Protos::Common::Entries& entries = dynamic_cast<const Protos::Common::Entries&>(message);
   emit result(entries);
}
