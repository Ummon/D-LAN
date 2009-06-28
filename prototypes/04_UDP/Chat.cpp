#include "Chat.h"

#include <QtCore/QDebug>

Chat::Chat()
{
   this->socket = new QUdpSocket(this);
     
   if (!this->socket->bind(Chat::port))
      qDebug() << "Can't bind" << endl;
   
   if (!connect(this->socket, SIGNAL(readyRead()), this, SLOT(processPendingDatagrams())))
      qDebug() << "Can't listen" << endl;
}

Chat::~Chat()
{
   delete this->socket;
}

void Chat::sendMessage(const QString& mess)
{
   QByteArray datagram = mess.toUtf8();
   if (this->socket->writeDatagram(
      datagram.data(),
      datagram.size(),
      QHostAddress::Broadcast,
      Chat::port
   ) == -1)
      qDebug() << "Unable to send datagram";      
}

void Chat::processPendingDatagrams()
{
   QTextStream out(stdout);
   while (this->socket->hasPendingDatagrams())
   {
      QByteArray datagram;
      datagram.resize(this->socket->pendingDatagramSize());
      this->socket->readDatagram(datagram.data(), datagram.size());
      out << "Message received : " << datagram.data() << endl;
   }
}
