#include <priv/FileManager.h>
using namespace FM;

#include <QSharedPointer>
#include <QStringList>
#include <QList>
#include <QDir>
#include <QRegExp>
#include <QMutableListIterator>

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/ILogger.h>
#include <Common/Math.h>
#include <priv/Cache/Entry.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

QSharedPointer<LM::ILogger> FileManager::logger(LM::Builder::newLogger("FileManager"));

FileManager::FileManager()
   : fileUpdater(this), cache(this, &this->fileUpdater)
{
   FileManager::logger->log("Loading ..", LM::EndUser);

   connect(&this->cache, SIGNAL(entryAdded(Entry*)), this, SLOT(entryAdded(Entry*)), Qt::DirectConnection);
   connect(&this->cache, SIGNAL(chunkAdded(Chunk*)), this, SLOT(chunkAdded(Chunk*)), Qt::DirectConnection);

   this->fileUpdater.start();
   FileManager::logger->log("Loaded!", LM::EndUser);
}

QStringList FileManager::getSharedDirsReadOnly()
{
   return this->cache.getSharedDirs(SharedDirectory::READ_ONLY);
}

QStringList FileManager::getSharedDirsReadWrite()
{
   return this->cache.getSharedDirs(SharedDirectory::READ_WRITE);
}

void FileManager::setSharedDirsReadOnly(const QStringList& dirs)
{
   this->cache.setSharedDirs(dirs, SharedDirectory::READ_ONLY);
}

void FileManager::setSharedDirsReadWrite(const QStringList& dirs)
{
   this->cache.setSharedDirs(dirs, SharedDirectory::READ_WRITE);
}

IChunk* FileManager::getChunk(const Common::Hash& hash)
{
   return this->chunks.value(hash);
}

/**
  * See http://dev.euphorik.ch/wiki/pmp/Algorithms#Word-indexing for more information.
  */
Protos::Common::FindResult FileManager::find(const QString& words)
{
   QStringList terms = FileManager::splitInWords(words);
   int n = terms.size();

   // Launch a search for each term.
   QSet<Entry*> results[n];
   for (int i = 0; i < n; i++)
      results[i] += this->wordIndex.search(terms[i]);

   Protos::Common::FindResult result;
   int level = 0;
   // For each group of intersection number.
   // For example, [a, b, c] :
   //  1) a & b & c
   //  2) (a & b) \ c
   //     (a & c) \ b
   //     (b & c) \ a
   //  3) a \ b \ c
   for (int i = 0; i < n; i++)
   {
      int nbIntersect = n - i; // Number of set intersected.
      int intersect[nbIntersect]; // A array of the results wich will be intersected.
      for (int j = 0; j < nbIntersect; j++)
         intersect[j] = j;

      // For each combination of the current intersection group.
      // For 2 intersections (nbIntersect == 2) among 3 elements [a, b, c] :
      //  * (a, b)
      //  * (a, c)
      //  * (b, c)
      for (int j = 0; j < Common::Math::nCombinations(n, nbIntersect); j++)
      {
         QSet<Entry*> currentLevelSet;

         // Apply intersects.
         currentLevelSet = results[intersect[0]];
         for (int k = 1; k < nbIntersect; k++)
            currentLevelSet &= results[intersect[k]];

         // Apply substracts.
         for (int k = -1; k < nbIntersect; k++)
         {
            for (
               int l = (k == -1 ? 0 : intersect[k] + 1);
                l < (k == nbIntersect - 1 ? n : intersect[k+1]);
                l++
            )
               currentLevelSet -= results[l];
         }

         // Populate the result.
         for (QSetIterator<Entry*> k(currentLevelSet); k.hasNext();)
         {
            Entry* entry = k.next();
            if (Directory* dir = dynamic_cast<Directory*>(entry))
            {
               Protos::Common::FindResult_DirEntryLevel* dirEntry = result.add_dirs();
               dirEntry->set_level(level);
               dir->populateDirEntry(dirEntry->mutable_dir());
            }
            else if (File* file  = dynamic_cast<File*>(entry))
            {
               Protos::Common::FindResult_FileEntryLevel* fileEntry = result.add_files();
               fileEntry->set_level(level);
               file->populateFileEntry(fileEntry->mutable_file());
            }
         }

         // Define positions of each intersect term.
         for (int k = nbIntersect - 1; k >= 0; k--)
            if  (intersect[k] < n - nbIntersect + k)
            {
               intersect[k] += 1;
               for (int l = k + 1; l < nbIntersect; l++)
                  intersect[l] = intersect[k] + (l - k);
               break;
            }

         level += 1;
      }
   }

   return result;
}

QBitArray FileManager::haveChunks(const QList<Common::Hash>& hashes)
{
   QBitArray result(hashes.size());
   for (int i = 0; i < hashes.size(); i++)
      result.setBit(i, this->chunks.contains(hashes[i]));
   return result;
}

quint64 FileManager::getAmount()
{
   return this->cache.getAmount();
}

QSharedPointer<IFile> FileManager::newFile(const Protos::Common::FileEntry& remoteEntry)
{
   // TODO...
   Directory* dir;
   QString name("plop");
   int size = 42;
   Common::Hashes hashes;
   return QSharedPointer<IFile>(new File(dir, name, size, hashes));
}

void FileManager::entryAdded(Entry* entry)
{
   LOG_DEBUG("Indexing item : " + entry->getFullPath());
   this->wordIndex.addItem(FileManager::splitInWords(entry->getName()), entry);

   /* See 'chunkAdded'
   if (File* file = dynamic_cast<File*>(entry))
   {
      for (QListIterator<Chunk*> i(file->getChunksRef()); i.hasNext();)
         this->chunks.add(i.next());
   }*/
}

void FileManager::entryRemoved(Entry* entry)
{
   // TODO
}

void FileManager::chunkAdded(Chunk* chunk)
{
   this->chunks.add(chunk);
}

QStringList FileManager::FileManager::splitInWords(const QString& words)
{
   const static QRegExp regExp("(\\W+|_)");
   QStringList keywords = words.toLower().split(regExp, QString::SkipEmptyParts);

   // Remove all word smaller than MAX_WORD_LENGTH.
   int i = 0;
   while (i < keywords.length())
   {
      if (keywords[i].length() < MAX_WORD_LENGTH)
         keywords.removeAt(i);
      else
         i += 1;
   }

   return keywords;
}

