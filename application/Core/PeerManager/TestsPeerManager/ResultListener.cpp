/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <ResultListener.h>
using namespace PM;

#include <QtDebug>
#include <QTest>
#include <QString>

#include <Common/ProtoHelper.h>
#include <Common/Network.h>

#include <ISocket.h>

ResultListener::ResultListener()
   : nbHashes(0), currentHash(0)
{
}

QList<Protos::Common::Entries> ResultListener::getEntriesResultList() const
{
   return this->entriesResultList;
}

int ResultListener::getNbEntriesResultReceived() const
{
   return this->entriesResultList.size();
}

void ResultListener::entriesResult(const Protos::Common::Entries& entries)
{
   this->entriesResultList << entries;
   qDebug() << "ResultListener::entriesResult : " << Common::ProtoHelper::getDebugStr(entries);
}

void ResultListener::result(const Protos::Core::GetHashesResult& result)
{
   this->nbHashes = result.nb_hash();
   this->currentHash = 0;
   qDebug() << "ResultListener::result : " << Common::ProtoHelper::getDebugStr(result);
}

void ResultListener::nextHash(const Common::Hash& hash)
{
   qDebug() << "ResultListener::nextHash : [" << this->currentHash + 1 << "/" << this->nbHashes << "] " << hash.toStr();
   this->currentHash++;
}

void ResultListener::result(const Protos::Core::GetChunkResult& result)
{
   qDebug() << "ResultListener::result : " << Common::ProtoHelper::getDebugStr(result);
}

static const QByteArray CHUNK_DATA("HELLO");

void ResultListener::stream(QSharedPointer<PM::ISocket> socket)
{
   QByteArray data = socket->getQSocket()->readAll();
   qDebug() << "ResultListener::stream : " << data;
   QCOMPARE(data, CHUNK_DATA);
   socket->finished();
}

void ResultListener::getChunk(Common::Hash hash, int offset, QSharedPointer<ISocket> socket)
{
   Protos::Core::GetChunkResult result;
   result.set_status(Protos::Core::GetChunkResult_Status_OK);
   socket->send(Common::Network::CORE_GET_CHUNK_RESULT, result);

   socket->getQSocket()->write(CHUNK_DATA);
   socket->finished();
}
