#include <priv/GetChunkResult.h>
using namespace PM;

#include <Common/Settings.h>

#include <priv/Log.h>

GetChunkResult::GetChunkResult(const Protos::Core::GetChunk& chunk, QSharedPointer<Socket> socket)
   : IGetChunkResult(SETTINGS.get<quint32>("socket_timeout")), chunk(chunk), socket(socket), error(false)
{
}

GetChunkResult::~GetChunkResult()
{
   this->socket->finished(this->error | this->isTimeouted());
}

void GetChunkResult::start()
{
   connect(this->socket.data(), SIGNAL(newMessage(quint32, const google::protobuf::Message&)), this, SLOT(newMessage(quint32, const google::protobuf::Message&)));
   socket->send(0x51, this->chunk);
   this->startTimer();
}

void GetChunkResult::newMessage(quint32 type, const google::protobuf::Message& message)
{
   if (type != 0x52)
      return;

   this->stopTimer();

   const Protos::Core::GetChunkResult& chunkResult = dynamic_cast<const Protos::Core::GetChunkResult&>(message);
   emit result(chunkResult);

   if (chunkResult.status() == Protos::Core::GetChunkResult_Status_OK)
   {
      emit stream(this->socket);
   }
   else
   {
      this->error = true;
      disconnect(this->socket.data(), SIGNAL(newMessage(quint32, const google::protobuf::Message&)), this, SLOT(newMessage(quint32, const google::protobuf::Message&)));
   }
}
