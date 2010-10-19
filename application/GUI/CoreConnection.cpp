#include "CoreConnection.h"
using namespace GUI;

#include <Common/ZeroCopyStreamQIODevice.h>

CoreConnection::CoreConnection()
{
   connect(&this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(&this->socket, SIGNAL(connected()), this, SIGNAL(coreConnected()));
   connect(&this->socket, SIGNAL(disconnected()), this, SIGNAL(coreDisconnected()));
}

void CoreConnection::connectToCore()
{
   this->socket.connectToHost("localhost", 59485);
}

void CoreConnection::dataReceived()
{
   // TODO : it will loop infinetly if not enough data is provided.
   /*while (!this->socket.atEnd())
   {*/
      if (this->currentHeader.isNull() && this->socket.bytesAvailable() >= Common::Network::HEADER_SIZE)
         this->currentHeader = Common::Network::readHeader(this->socket);

      if (!this->currentHeader.isNull() && this->socket.bytesAvailable() >= this->currentHeader.size)
      {
         this->readMessage();
         this->currentHeader.setNull();
      }
   //}
}

bool CoreConnection::readMessage()
{
   bool readOK = false;

   switch (this->currentHeader.type)
   {
   case 0x01 : // State.
      {
         Protos::GUI::State state;

         // This scope (and the others ones below) is here to force the input stream to read all the bytes.
         // See Common::ZeroCopyInputStreamQIODevice::~ZeroCopyInputStreamQIODevice.
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(&this->socket);
            readOK = state.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         emit newState(state);
      }
      break;

   default:
      readOK = false;
   }

   return readOK;
}
