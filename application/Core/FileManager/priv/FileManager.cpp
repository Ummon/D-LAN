#include <priv/FileManager.h>
using namespace FileManager;

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

QSharedPointer<LogManager::ILogger> FileManager::FileManager::logger(LogManager::Builder::newLogger("FileManager"));

::FileManager::FileManager()
   : fileUpdater(this)
{
   FileManager::logger->log("Loading ..", LogManager::EndUser);
   this->fileUpdater.start();
   FileManager::logger->log("Loaded!", LogManager::EndUser);
}

QStringList FileManager::FileManager::getSharedDirsReadOnly()
{
   return this->getSharedDirs(SharedDirectory::READ_ONLY);
}

QStringList FileManager::FileManager::getSharedDirsReadWrite()
{
   return this->getSharedDirs(SharedDirectory::READ_WRITE);
}

void ::FileManager::setSharedDirsReadOnly(const QStringList& dirs)
{
   this->setSharedDirs(dirs, SharedDirectory::READ_ONLY);
}

void ::FileManager::setSharedDirsReadWrite(const QStringList& dirs)
{
   this->setSharedDirs(dirs, SharedDirectory::READ_WRITE);
}

IChunk* ::FileManager::getChunk(const Common::Hash& hash)
{
   return this->chunks.value(hash);
}

/**
  * TODO : little explanation? ^.^
  */
Protos::Common::FindResult FileManager::FileManager::find(const QString& words)
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

Chunks& ::FileManager::getChunks()
{
   return this->chunks;
}

void FileManager::FileManager::addToWordIndex(Entry* entry)
{
   LOG_DEBUG("Indexing item " + entry->getPath());
   this->wordIndex.addItem(FileManager::splitInWords(entry->getName()), entry);
}

QStringList FileManager::FileManager::getSharedDirs(SharedDirectory::Rights rights)
{
   QStringList list;

   for (QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* dir = i.next();
      if (dir->getRights() == rights)
         list << dir->getPath();
   }
   return list;
}

void ::FileManager::setSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights)
{
   // Filter the actual shared directories by looking theirs rights.
   QList<SharedDirectory*> sharedDirs;
   for(QListIterator<SharedDirectory*> i(this->sharedDirs); i.hasNext();)
   {
      SharedDirectory* dir = i.next();
      if (dir->getRights() == rights)
         sharedDirs << dir;
   }

   QStringList newDirs;
   QMutableListIterator<SharedDirectory*> j(sharedDirs);

   // O(n^2).
   for(QListIterator<QString> i(dirs); i.hasNext();)
   {
      QString dir =  QDir::cleanPath(i.next());
      j.toFront();
      while(j.hasNext())
      {
         if (j.next()->getPath() == dir)
         {
            j.remove();
            goto next;
         }
      }
      newDirs << dir;
      next:;
   }

   // Remove shared directories.
   for (j.toFront(); j.hasNext();)
   {
      SharedDirectory* dir = j.next();
      LOG_DEBUG("Remove a shared directory : " + dir->getPath());
      this->fileUpdater.rmRoot(dir);
      this->sharedDirs.removeOne(dir);
   }

   // Create new shared directories.
   for (QListIterator<QString> i(newDirs); i.hasNext();)
   {
      QString path = i.next();
      SharedDirectory* dir = new SharedDirectory(this, path);
      LOG_DEBUG("Add a shared directory : " + dir->getPath());
      this->fileUpdater.addRoot(dir);
      this->sharedDirs << dir;
   }
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

