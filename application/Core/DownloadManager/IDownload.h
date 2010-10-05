#ifndef DOWNLOADMANAGER_IDOWNLOAD_H
#define DOWNLOADMANAGER_IDOWNLOAD_H

#include <QObject>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace DM
{
   enum Status
   {
     COMPLETE,
     NO_SOURCE,
     NOT_FOUND,
     UNKNOWN_PEER,
     INITIALIZING,
     DOWNLOADING,
     QUEUED
   };

   class IDownload : public QObject
   {
   public:
      virtual ~IDownload() {}

      virtual int getId() = 0;
      virtual Status getStatus() = 0;

      /**
        * Return a value between 0 and 100.
        */
      virtual char getProgress() = 0;

      virtual Common::Hash getPeerSourceID() = 0;
      virtual Protos::Common::Entry getEntry() = 0;

      /**
        * Stop and delete the download.
        */
      virtual void remove() = 0;
   };
}
#endif
