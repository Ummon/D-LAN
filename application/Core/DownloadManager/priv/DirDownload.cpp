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

#include <priv/DirDownload.h>
using namespace DM;

#include <Common/ProtoHelper.h>

#include <Core/FileManager/Exceptions.h>

#include <priv/Log.h>
#include <priv/Constants.h>

/**
  * @class DM::DirDownload
  *
  * A DirDownload will try, when the method 'retrieveEntries' is called, to retrieve all the sub entries of a remote directory.
  * The sub entries can be a mix of files and directories.
  * Once the content is know, the signal 'newEntries' is emitted.
  */

DirDownload::DirDownload(
   QSharedPointer<FM::IFileManager> fileManager,
   OccupiedPeers& occupiedPeersAskingForEntries,
   PM::IPeer* peerSource,
   const Protos::Common::Entry& remoteEntry,
   const Protos::Common::Entry& localEntry
) :
   Download(fileManager, peerSource, remoteEntry, localEntry),
   occupiedPeersAskingForEntries(occupiedPeersAskingForEntries)
{
   L_DEBU(QString("New DirDownload: source = %1, remoteEntry : \n%2\nlocalEntry : \n%3").
      arg(this->peerSource->toStringLog()).
      arg(Common::ProtoHelper::getDebugStr(this->remoteEntry)).
      arg(Common::ProtoHelper::getDebugStr(this->localEntry))
   );
}

DirDownload::~DirDownload()
{
   this->setStatus(DELETED);

   this->occupiedPeersAskingForEntries.setPeerAsFree(this->peerSource);

   this->getEntriesResult.clear();
}

void DirDownload::start()
{
   this->updateStatus();
   this->createDirectory();
   this->retrieveEntries();
}

/**
  * Ask the DirDownload to get its content.
  * The signal 'newEntries' will be emitted when the answer is received.
  */
bool DirDownload::retrieveEntries()
{
   if (this->isStatusErroneous() || this->status == ENTRY_NOT_FOUND || this->status == DELETED)
      return false;

   Protos::Core::GetEntries getEntries;
   getEntries.mutable_dirs()->add_entry()->CopyFrom(this->remoteEntry);
   this->getEntriesResult = this->peerSource->getEntries(getEntries);

   if (this->getEntriesResult.isNull() || !this->occupiedPeersAskingForEntries.setPeerAsOccupied(this->peerSource))
   {
      this->getEntriesResult.clear();
      return false;
   }

   connect(this->getEntriesResult.data(), &PM::IGetEntriesResult::result, this, &DirDownload::result);
   connect(this->getEntriesResult.data(), &PM::IGetEntriesResult::timeout, this, &DirDownload::resultTimeout);
   this->getEntriesResult->start();

   return true;
}

bool DirDownload::updateStatus()
{
   if (Download::updateStatus())
      return true;

   if (!this->peerSource->isAvailable())
      this->setStatus(UNKNOWN_PEER_SOURCE);
   else
      this->setStatus(QUEUED);

   return false;
}

void DirDownload::retryToGetEntries()
{
   this->setStatus(QUEUED);
   this->retrieveEntries();
}

void DirDownload::result(const Protos::Core::GetEntriesResult& entries)
{
   // We asked for one directory, we shouldn't have zero result.
   if (entries.result_size() > 0 && entries.result(0).status() == Protos::Core::GetEntriesResult::EntryResult::OK)
   {
      Protos::Common::Entries entriesCopy;

      if (entries.result(0).has_entries())
      {
         entriesCopy.CopyFrom(entries.result(0).entries());
         // We need to specify the shared directory for each entry.
         for (int i = 0; i < entriesCopy.entry_size(); i++)
            entriesCopy.mutable_entry(i)->mutable_shared_entry()->CopyFrom(this->remoteEntry.shared_entry());
      }

      this->getEntriesResult.clear();
      emit newEntries(entriesCopy);
   }
   else
   {
      if (entries.result_size() == 0 || entries.result(0).status() == Protos::Core::GetEntriesResult::EntryResult::DONT_HAVE)
      {
         L_DEBU("Unable to get the entries: ENTRY_NOT_FOUND");
         this->setStatus(ENTRY_NOT_FOUND);
      }
      else if (entries.result(0).status() == Protos::Core::GetEntriesResult::EntryResult::TIMEOUT_SCANNING_IN_PROGRESS)
      {
         L_DEBU("Unable to get the entries: REMOTE_SCANNING_IN_PROGRESS");
         this->setStatus(REMOTE_SCANNING_IN_PROGRESS);
      }
      else
      {
         L_DEBU("Unable to get the entries: UNABLE_TO_GET_ENTRIES");
         this->setStatus(UNABLE_TO_GET_ENTRIES);
      }

      this->getEntriesResult.clear();
      this->occupiedPeersAskingForEntries.setPeerAsFree(this->peerSource);
      QTimer::singleShot(RETRY_GET_ENTRIES_PERIOD, this, SLOT(retryToGetEntries()));
   }
}

void DirDownload::resultTimeout()
{
   L_DEBU("Unable to retrieve the entries: timeout");
   this->setStatus(UNABLE_TO_GET_ENTRIES);

   this->getEntriesResult.clear();
   this->occupiedPeersAskingForEntries.setPeerAsFree(this->peerSource);
   QTimer::singleShot(RETRY_GET_ENTRIES_PERIOD, this, SLOT(retryToGetEntries()));
}

void DirDownload::createDirectory()
{
   // Only create the directory if it's empty. In other cases the directories are created when the file is created.
   if (this->remoteEntry.is_empty())
   {
      try
      {
         this->fileManager->newDirectory(this->localEntry);
      }
      catch (FM::NoWriteableDirectoryException&)
      {
         L_DEBU(QString("There is no shared directory with writing rights for this download: %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
         this->setStatus(NO_SHARED_DIRECTORY_TO_WRITE);
      }
      catch (FM::UnableToCreateNewDirException&)
      {
         L_DEBU(QString("Unable to create the directory, download: %1").arg(Common::ProtoHelper::getStr(this->remoteEntry, &Protos::Common::Entry::name)));
         this->setStatus(UNABLE_TO_CREATE_THE_DIRECTORY);
      }
   }
}
