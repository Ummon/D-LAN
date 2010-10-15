#ifndef DOWNLOADMANAGER_IDOWNLOAD_H
#define DOWNLOADMANAGER_IDOWNLOAD_H

#include <QObject>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace DM
{
   enum Status
   {
     QUEUED = 1,
     INITIALIZING,
     DOWNLOADING,
     COMPLETE,
     PAUSED,

     // All theses status will imply the paused status.
     UNKNOWN_PEER, // The source peer can't be found.
     ENTRY_NOT_FOUND, // The source peer can't find the entry.
     NO_SOURCE, // Some chunk can't be downloaded. Only when there is no more downloading.
   };

   class IDownload
   {
   public:
      virtual ~IDownload() {}

      virtual int getId() const = 0;

      virtual Status getStatus() const = 0;

      /**
        * Return a value between 0 and 100.
        */
      virtual int getProgress() const = 0;

      virtual Common::Hash getPeerSourceID() = 0;

      virtual Protos::Common::Entry getEntry() = 0;

      /**
        * Stop and delete the download.
        */
      virtual void remove() = 0;
   };
}
#endif
