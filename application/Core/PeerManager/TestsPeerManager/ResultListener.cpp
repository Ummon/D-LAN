/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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

#include <ISocket.h>

ResultListener::ResultListener() :
   nbHashes(0), currentHash(0), streamReceived(false)
{
}

QList<Protos::Core::GetEntriesResult> ResultListener::getEntriesResultList() const
{
   return this->entriesResultList;
}

/**
  * Return the lest number of received entries.
  * @param n The nth received entry.
  */
int ResultListener::getNbEntriesResultReceived(int n) const
{
   if (n >= this->entriesResultList.size())
      return 0;

   return this->entriesResultList.last().entries(n).entry_size();
}

const Protos::Core::GetHashesResult& ResultListener::getLastGetHashesResult()
{
   return this->lastGesHashesResult;
}

const Common::Hash& ResultListener::getLastReceivedHash()
{
   return this->lastHashReceived;
}

quint32 ResultListener::getNbHashReceivedFromLastGetHashes()
{
   return this->currentHash;
}

bool ResultListener::isStreamReceived()
{
   return this->streamReceived;
}

void ResultListener::entriesResult(const Protos::Core::GetEntriesResult& result)
{
   this->entriesResultList << result;
   qDebug() << "ResultListener::entriesResult : " << Common::ProtoHelper::getDebugStr(result);
}

void ResultListener::result(const Protos::Core::GetHashesResult& result)
{
   this->nbHashes = result.nb_hash();
   this->currentHash = 0;
   qDebug() << "ResultListener::result : " << Common::ProtoHelper::getDebugStr(result);
}

void ResultListener::nextHash(const Common::Hash& hash)
{
   this->lastHashReceived = hash;
   qDebug() << "ResultListener::nextHash : [" << this->currentHash + 1 << "/" << this->nbHashes << "] " << hash.toStr();
   this->currentHash++;
}

void ResultListener::result(const Protos::Core::GetChunkResult& result)
{
   qDebug() << "ResultListener::result : " << Common::ProtoHelper::getDebugStr(result);
}

static const QByteArray CHUNK_DATA("ALL YOUR BYTE ARE BELONG TO US");

void ResultListener::stream(QSharedPointer<PM::ISocket> socket)
{
   QByteArray data = socket->readAll();
   qDebug() << "ResultListener::stream : " << data;
   QCOMPARE(data, CHUNK_DATA);
   socket->finished();
   this->streamReceived = true;
}

void ResultListener::getChunk(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<ISocket> socket)
{
   socket->write(CHUNK_DATA);
   socket->finished();
}
