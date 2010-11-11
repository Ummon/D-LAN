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
   // We must disconnect because 'this->socket->finished' can read some data and emit 'newMessage'.
   disconnect(this->socket.data(), SIGNAL(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)));
   this->socket->finished(this->error | this->isTimeouted());
}

void GetChunkResult::start()
{
   connect(this->socket.data(), SIGNAL(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), Qt::DirectConnection);
   socket->send(Common::Network::CORE_GET_CHUNK, this->chunk);
   this->startTimer();
}

void GetChunkResult::setError()
{
   this->error = true;
}

void GetChunkResult::newMessage(Common::Network::CoreMessageType type, const google::protobuf::Message& message)
{
   if (type != Common::Network::CORE_GET_CHUNK_RESULT)
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
      disconnect(this->socket.data(), SIGNAL(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)), this, SLOT(newMessage(Common::Network::CoreMessageType, const google::protobuf::Message&)));
   }
}
