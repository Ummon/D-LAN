#ifndef DOWNLOADMANAGER_IDOWNLOAD_H
#define DOWNLOADMANAGER_IDOWNLOAD_H

#include <QObject>
#include <QSet>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace DM
{
   enum Status
   {
     QUEUED = 0x1,
     INITIALIZING = 0x2,
     DOWNLOADING = 0x3,
     COMPLETE = 0x4,
     PAUSED = 0x5,

     // All theses status will imply the paused status.
     UNKNOWN_PEER = 0x10, // The source peer can't be found.
     ENTRY_NOT_FOUND = 0x11, // The source peer can't find the entry.
     NO_SOURCE = 0x12, // Some chunk can't be downloaded. Only when there is no more downloading.

     // Error status :
     NO_SHARED_DIRECTORY_TO_WRITE = 0x20,
     NO_ENOUGH_FREE_SPACE = 0x21,
     THE_FILE_ALREADY_EXISTS = 0x22,
     UNABLE_TO_CREATE_THE_FILE = 0x23,
     UNABLE_TO_RETRIEVE_THE_HASHES = 0x24,
   };

   class IDownload
   {
   public:
      virtual ~IDownload() {}

      virtual quint64 getID() const = 0;

      virtual Status getStatus() const = 0;

      /**
        * Return a value between 0 and 100.
        */
      virtual int getProgress() const = 0;

      virtual Common::Hash getPeerSourceID() const = 0;

      /**
        * Return all the peer who own at least one chunk.
        */
      virtual QSet<Common::Hash> getPeers() const = 0;

      virtual Protos::Common::Entry getEntry() = 0;

      /**
        * Stop and delete the download.
        * If the download has begun the downloaded data are removed.
        */
      virtual void remove() = 0;
   };
}
#endif
