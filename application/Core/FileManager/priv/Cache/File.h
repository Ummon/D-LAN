#ifndef FILEMANAGER_FILE_H
#define FILEMANAGER_FILE_H

#include <exception>

#include <QString>
#include <QMutex>
#include <QWaitCondition>
#include <QFile>
#include <QFileInfo>
#include <QList>
#include <QSharedPointer>
#include <QDateTime>

#include <Common/Hashes.h>
#include <Protos/common.pb.h>
#include <Protos/files_cache.pb.h>
#include <priv/Cache/Entry.h>

namespace FM
{
   class IChunk;
   class Chunk;
   class Directory;
   class Cache;

   class File : public Entry
   {
   public:
      File(
         Directory* dir,
         const QString& name,
         qint64 size,
         const QDateTime& dateLastModified,
         const Common::Hashes& hashes = Common::Hashes(),
         bool createPhysically = false
      );

      virtual ~File();

      bool restoreFromFileCache(const Protos::FileCache::Hashes_File& file);

      void populateHashesFile(Protos::FileCache::Hashes_File& fileToFill) const;

      void populateFileEntry(Protos::Common::FileEntry* entry) const;

      QString getPath() const;
      QString getFullPath() const;
      Directory* getRoot() const;
      QDateTime getDateLastModified() const;

      void newDataWriterCreated();
      void newDataReaderCreated();

      void dataWriterDeleted();
      void dataReaderDeleted();

      bool write(const QByteArray& buffer, qint64 offset);
      qint64 read(QByteArray& buffer, qint64 offset);

      bool computeHashes(int n = 0);

      void stopHashing();

      QList< QSharedPointer<Chunk> > getChunks() const;

      bool hasAllHashes();
      bool hasOneOrMoreHashes();

      bool isComplete();
      bool correspondTo(const QFileInfo& fileInfo);

      void physicallyRemoveUnfinished();

      void changeDirectory(Directory* dir);

   private:
      int getNbChunks();

      Directory* dir;
      QList< QSharedPointer<Chunk> > chunks;
      QDateTime dateLastModified;

      int numDataWriter;
      int numDataReader;
      QFile* fileInWriteMode;
      QFile* fileInReadMode;
      QMutex writeLock; ///< Protect the file from concurrent access from different downloaders.
      QMutex readLock; ///< Protect the file from concurrent access from different uploaders.

      // Mutex and wait condition used during hashing.
      // (TODO : It's a bit heavy, try to reduce the memory footprint).
      bool hashing;
      bool toStopHashing;
      QWaitCondition hashingStopped;
      QMutex hashingMutex;
   };
}
#endif
