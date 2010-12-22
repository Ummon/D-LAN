/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef TESTS_FILEMANAGER_STRESSTEST_H
#define TESTS_FILEMANAGER_STRESSTEST_H

#include <QSharedPointer>
#include <QDir>
#include <QThread>

#include <Libs/MersenneTwister.h>

#include <IFileManager.h>
#include <IGetHashesResult.h>
#include <IDataWriter.h>
#include <IDataReader.h>
#include <IChunk.h>
using namespace FM;

class RandGenerator
{
public:
   int percentRand();
   int permilRand();
   int rand(int n);
   QString generateAName();

private:
   MTRand mtrand;
};

class StressTest;

class FilesAndDirs : public QThread
{
public:
   FilesAndDirs(QSharedPointer<IFileManager>, StressTest* stressTest);
   void run();

private:
   static void (FilesAndDirs::*actions[])();
   static const int NB_ACTION;

   void createADir();
   void removeADir();
   void createAFile();
   void removeAFile();

   QSharedPointer<IFileManager> fileManager;
   StressTest* stressTest;

   QStringList directories;
   QStringList dirsToDelete;

   RandGenerator randGen;
};

class Downloader;
class Uploader;

class StressTest : public QObject
{
   Q_OBJECT
   static const int NB_FILES_AND_DIR_THREAD;
public:
   StressTest();
   QStringList getSharedDirsReadOnly() const;
   QStringList getSharedDirsReadWrite() const;

private:
   static void (StressTest::*actions[])();
   static const int NB_ACTION;

   void createASharedDir();
   void removeASharedDir();
   void doASearch();
   void printAmount();
   void getChunk();
   void newFile();
   void haveChunk();
   void getRootEntries();
   void getEntries();
   void getHashes();

private slots:
   void nextHash(Common::Hash hash);

private:
   void addEntries(const Protos::Common::Entries& entries);

   static QString entryToStr(const Protos::Common::Entry& entry);

   QSharedPointer<IFileManager> fileManager;
   QList<FilesAndDirs*> filesAndDirs;
   QStringList sharedDirsReadOnly;
   QStringList sharedDirsReadWrite;
   QStringList dirsToDelete;

   QList<Common::Hash> someHashes;
   QList<Protos::Common::Entry> knownDirEntries;
   QList<Protos::Common::Entry> knownFileEntries;

   struct HashesResult
   {
      HashesResult(QSharedPointer<IGetHashesResult> result, int nb, QString filename)
         : result(result), nb(nb), filename(filename) {}
      QSharedPointer<IGetHashesResult> result;
      int nb;
      QString filename;
   };
   QList< HashesResult > getHashesResults;

   RandGenerator randGen;
};

class Downloader : public QThread
{
public:
   Downloader(QSharedPointer<IChunk> chunk, QString filePath);

protected:
   void run();

private:
   QSharedPointer<IChunk> chunk;
   QString filePath;
   RandGenerator randGen;
};

class Uploader : public QThread
{
public:
   Uploader(QSharedPointer<IChunk> chunk);

protected:
   void run();

private:
   QSharedPointer<IChunk> chunk;
   RandGenerator randGen;
};

#endif
