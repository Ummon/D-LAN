#ifndef FILEMANAGER_FILEHASHER_H
#define FILEMANAGER_FILEHASHER_H

#include <QObject>
#include <QMutex>
#include <QWaitCondition>

#include <Common/Uncopyable.h>

namespace FM
{
   class Entry;
   class File;

   class FileHasher : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      FileHasher();

      bool start(File* fileCache, int n = 0, int* amountHashed = 0);
      void stop();

   private slots:
      void entryRemoved(Entry* entry);

   private:
      File* currentFileCache;

      bool hashing;
      bool toStopHashing;
      QWaitCondition hashingStopped;
      QMutex hashingMutex;
   };
}

#endif
