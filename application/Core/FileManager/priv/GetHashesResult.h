#ifndef GETHASHESRESULT_H
#define GETHASHESRESULT_H

#include <QObject>
#include <QMutex>

#include <Protos/core_protocol.pb.h>

#include <IGetHashesResult.h>
#include <priv/Cache/Cache.h>
#include <priv/FileUpdater/FileUpdater.h>

namespace FM
{
   class Cache;
   class File;
   class FileUpdater;

   class GetHashesResult : public IGetHashesResult
   {
      Q_OBJECT
   public:
      GetHashesResult(const Protos::Common::Entry& fileEntry, Cache& cache, FileUpdater& fileUpdater);
      ~GetHashesResult();
      Protos::Core::GetHashesResult start();

   private slots:
      void chunkHashKnown(QSharedPointer<Chunk> chunk);

   private:
      void sendNextHash(QSharedPointer<Chunk> chunk);

      const Protos::Common::Entry& fileEntry;
      File* file; // TODO : if the file is deleted how can we know?
      Cache& cache;
      FileUpdater& fileUpdater;

      QMutex mutex;
      int nbHash;
      int lastHashNumSent;
   };
}

#endif
