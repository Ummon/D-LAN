#include <priv/GetChunkResult.h>
using namespace PM;

#include <priv/Log.h>

GetChunkResult::GetChunkResult(const Protos::Core::GetChunk& chunk, Socket* socket)
   : chunk(chunk), socket(socket)
{
}

GetChunkResult::~GetChunkResult()
{
   this->socket->finished();
}

void GetChunkResult::start()
{
   connect(this->socket, SIGNAL(newMessage(quint32, const google::protobuf::Message&)), this, SLOT(newMessage(quint32, const google::protobuf::Message&)));
   socket->send(0x51, this->chunk);
}

void GetChunkResult::newMessage(quint32 type, const google::protobuf::Message& message)
{
   if (type != 0x52)
      return;

   const Protos::Core::GetChunkResult& chunkResult = dynamic_cast<const Protos::Core::GetChunkResult&>(message);
   emit result(chunkResult);

   if (chunkResult.status() == Protos::Core::GetChunkResult_Status_OK)
   {
      this->socket->stopListening();
      emit stream(this->socket);
   }
   else
   {
      disconnect(this->socket, SIGNAL(newMessage(quint32, const google::protobuf::Message&)), this, SLOT(newMessage(quint32, const google::protobuf::Message&)));
   }
}
