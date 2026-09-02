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

#pragma once

#include <QObject>
#include <QList>
#include <QSharedPointer>
#include <QString>

#include <Common/Hash.h>
#include <Common/Network/Message.h>

#include <Core/HashCache/IHashCache.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <Core/NetworkListener/INetworkListener.h>

/**
  * Two complete core instances are created in the same process, each one with its own ID.
  * They discover each other with the 'IMAlive' multicast messages (the multicast loopback is only enabled in debug),
  * then the unicast and the search paths are tested between them.
  */
class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();

   // Utils.
   void multicastGroupIPv4();
   void multicastGroupIPv6();
   void addressToListenTo();

   // Sending without any known peer.
   void sendToUnknownPeer();
   void sendMessageTooLarge();
   void sendMulticast();

   // Between the two instances.
   void peerDiscovery();
   void unicastReception();
   void search();

   void cleanupTestCase();

private:
   struct Instance
   {
      QSharedPointer<HC::IHashCache> hashCache;
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;
      QSharedPointer<NL::INetworkListener> networkListener; // Must be the last one to be destroyed first.
   };

   Instance createInstance(const Common::Hash& ID, const QString& nick);
   bool peersDiscovered() const;

   QList<Common::Hash> peerIDs;
   QList<Instance> instances;

   QList<Common::Message> receivedMessages; // Messages received by the first instance.
};
