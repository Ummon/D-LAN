#include <priv/GetChunkResult.h>
using namespace PM;

#include <Common/Settings.h>

#include <priv/Log.h>

GetChunkResult::GetChunkResult(const Protos::Core::GetChunk& chunk, QSharedPointer<Socket> socket)
   : chunk(chunk), socket(socket), error(false)
{
   this->timerTimeout.setInterval(SETTINGS.get<quint32>("socket_timeout"));
   this->timerTimeout.setSingleShot(true);
   connect(&this->timerTimeout, SIGNAL(timeout()), this, SLOT(timeoutError()));
}

GetChunkResult::~GetChunkResult()
{
   this->socket->finished(this->error);
}

void GetChunkResult::start()
{
   connect(this->socket.data(), SIGNAL(newMessage(quint32, const google::protobuf::Message&)), this, SLOT(newMessage(quint32, const google::protobuf::Message&)));
   socket->send(0x51, this->chunk);
   this->timerTimeout.start();
}

void GetChunkResult::newMessage(quint32 type, const google::protobuf::Message& message)
{
   if (type != 0x52)
      return;

   this->timerTimeout.stop(); // TODO : It seems a single shot time cannot be stopped...
   disconnect(&this->timerTimeout, SIGNAL(timeout()), this, SIGNAL(timeout()));

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

void GetChunkResult::timeoutError()
{
   this->error = true;
   emit timeout();
}
