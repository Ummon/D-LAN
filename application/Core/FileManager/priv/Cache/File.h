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

   /**
     * A file can be finished or unfinished.
     * If it is an unfinished one, the name ends with ".unfinished" (see UNFINISHED_SUFFIX_TERM).
     * When a file is just finished the suffix ".unfinished" is removed and the file is renamed.
     */
   class File : public Entry
   {
   public:

      /**
        * Create a new file into a given directory.
        * The file may or may not have a correponding local file.
        * If 'createPhysically' is true then the file is created as unfinished with no byte known.
        * @param hashes Optional hashes, if given it must contain ALL hashes.
        * @exception FileAlreadyExistsException : TODO!!!
        * @exception FilePhysicallyAlreadyExistsException : TODO!!!
        */
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

      /**
        * Write some bytes to the file at the given offset.
        * If the buffer exceed the file size then only the begining of the buffer is
        * used, the file is not resizing.
        * @param buffer The buffer.
        * @param offset An offset.
        * @return true if end of file reached.
        */
      bool write(const QByteArray& buffer, qint64 offset);

      /**
        * Fill the buffer with the read bytes from the given offset.
        * If the end of file is reached the buffer will be partialy filled.
        * @param buffer The buffer.
        * @param offset An offset.
        * @return the number of bytes read.
        */
      qint64 read(QByteArray& buffer, qint64 offset);

      /**
        * It will open the file, read it and calculate all theirs chunk hashes.
        * If it already owns some chunks, there are destroyed first.
        * This method can be called from an another thread than the main one. For example,
        * from 'FileUpdated' thread.
        * @return Return true if all the hashes as been computed.
        * @exception FileNotFoundException
        */
      bool computeHashes();

      void stopHashing();

      QList< QSharedPointer<Chunk> > getChunks() const;

      bool hasAllHashes();
      bool hasOneOrMoreHashes();

      bool isComplete();
      bool correspondTo(const QFileInfo& fileInfo);

      /**
        * Remove the file physically only if it's not complete.
        * The file removed must ended by UNFINISHED_SUFFIX_TERM.
        */
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
