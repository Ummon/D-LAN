/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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

#include <FindBenchmark.h>

#include <limits>

#include <QDateTime>
#include <QHash>
#include <QRandomGenerator>
#include <QtDebug>

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/Path.h>
#include <Common/Network/MessageHeader.h>

#include <priv/FileManager.h>
#include <priv/Cache/Chunk.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>

#include <BenchmarkUtils.h>
#include <MockHashCache.h>
using namespace FM;

namespace
{
   using Category = Protos::Common::FindPattern_Category;

   const int NB_DIRECTORIES = 15000;
   const int MAX_NB_FILES_PER_DIRECTORY = 40; ///< 20 on average: about 300'000 files.
   const int MAX_DEPTH = 4; ///< Of a directory in the shared one.

   const int MAX_NB_RESULT = 300; ///< The default value of the setting 'max_number_of_search_result_to_send'.
   const int MAX_SIZE = 163840 - Common::MessageHeader::HEADER_SIZE; ///< [byte]. Of a message of results: the default value of the setting 'udp_buffer_size' without the header.

   const qint64 KB = 1000;
   const qint64 MB = 1000 * KB;
   const qint64 GB = 1000 * MB;
   const qint64 NO_LIMIT = std::numeric_limits<qint64>::max();

   /**
     * One to three words.
     */
   QString createDirectoryName(QRandomGenerator& rng, const QStringList& vocabulary)
   {
      QStringList words;
      for (int i = 1 + rng.bounded(3); i > 0; --i)
         words << BenchmarkUtils::pickWord(rng, vocabulary);
      return words.join(' ');
   }

   /**
     * The size [byte] of a file depends on its type: the searches by size and by extension give different results.
     */
   qint64 createSize(QRandomGenerator& rng, const QString& name)
   {
      static const QHash<QString, QPair<qint64, qint64>> RANGES {
         { "txt", { 100, 500 * KB } },
         { "pdf", { 50 * KB, 30 * MB } },
         { "jpg", { 500 * KB, 8 * MB } },
         { "mp3", { 2 * MB, 15 * MB } },
         { "flac", { 15 * MB, 60 * MB } },
         { "avi", { 100 * MB, 700 * MB } },
         { "mkv", { 200 * MB, 1500 * MB } }
      };

      const QPair<qint64, qint64> range = RANGES.value(name.sliced(name.lastIndexOf('.') + 1));
      return range.first + static_cast<qint64>(rng.generateDouble() * (range.second - range.first));
   }
}

FindBenchmark::FindBenchmark() :
   fileManager(nullptr)
{
}

/**
  * Create a tree of directories and their files, all of them known and hashed as the ones of an up to date cache.
  */
void FindBenchmark::initTestCase()
{
#if DEBUG
   qWarning() << "Built in debug: the durations aren't the ones of a release";
#endif

   QVERIFY(this->sharedDirectory.isValid());
   const QString sharedPath = this->sharedDirectory.path() + '/';

   this->fileManager = new FileManager(QSharedPointer<HC::IHashCache>(new MockHashCache));
   this->fileManager->fileUpdater.stop(); // A scan would remove the entries: they don't exist in the shared directory.
   this->fileManager->addASharedPath(sharedPath);
   Directory* root = dynamic_cast<Directory*>(this->fileManager->getEntry(Common::Path(sharedPath)));
   QVERIFY(root);

   // Always the same entries: the results of two runs can be compared.
   QRandomGenerator rng(42);
   this->vocabulary = BenchmarkUtils::createVocabulary(rng);

   const QDateTime date(QDate(2020, 1, 1), QTime(12, 0));
   QList<Directory*> directories { root };
   QList<int> depths { 0 };
   int nbFiles = 0;
   quint32 nbChunks = 0;
   while (directories.size() <= NB_DIRECTORIES)
   {
      const int parent = rng.bounded(static_cast<int>(directories.size()));
      const QString directoryName = createDirectoryName(rng, this->vocabulary);
      if (depths.at(parent) == MAX_DEPTH || directories.at(parent)->getSubDir(directoryName))
         continue;

      // A directory is followed by its files, as a scan creates them.
      Directory* directory = directories.at(parent)->createSubDir(directoryName);
      directories << directory;
      depths << depths.at(parent) + 1;

      for (int i = rng.bounded(MAX_NB_FILES_PER_DIRECTORY + 1); i > 0; --i)
      {
         const QString name = BenchmarkUtils::createName(rng, this->vocabulary);
         if (directory->getFile(name))
            continue;

         const qint64 size = createSize(rng, name);
         QList<Common::Hash> hashes;
         for (qint64 offset = 0; offset < size; offset += Chunk::CHUNK_SIZE)
            hashes << Common::Hash::rand(nbChunks++);

         new File(directory->getRoot(), name, size, false, date, directory, hashes, false, false);
         ++nbFiles;
      }
   }

   qInfo() << nbFiles << "files," << NB_DIRECTORIES << "directories," << nbChunks << "chunks";
}

void FindBenchmark::find_data()
{
   QTest::addColumn<QString>("words");
   QTest::addColumn<QStringList>("extensions");
   QTest::addColumn<qint64>("minFileSize");
   QTest::addColumn<qint64>("maxFileSize");
   QTest::addColumn<int>("category");
   QTest::addColumn<bool>("found");

   const auto addRow = [](
      const char* name,
      const QString& words,
      const QStringList& extensions = QStringList(),
      qint64 minFileSize = 0,
      qint64 maxFileSize = NO_LIMIT,
      Category category = Protos::Common::FindPattern::FILE_DIR,
      bool found = true
   )
   {
      QTest::newRow(name) << words << extensions << minFileSize << maxFileSize << static_cast<int>(category) << found;
   };

   const QString common = this->vocabulary.at(0);
   const QString rare = this->vocabulary.at(30000);
   const QString threeWords = common + ' ' + this->vocabulary.at(50) + ' ' + rare;

   // Words only.
   addRow("unknown word", common + "x", {}, 0, NO_LIMIT, Protos::Common::FindPattern::FILE_DIR, false); // The most frequent case: a peer receives the searches of all the other ones.
   addRow("rare word", rare);
   addRow("common word", common);
   addRow("prefix of many words", "dsc");
   addRow("three words", threeWords);
   addRow("rare word and extension as a word", rare + " mp3");

   // Words and a category.
   addRow("common word, files", common, {}, 0, NO_LIMIT, Protos::Common::FindPattern::FILE);
   addRow("common word, directories", common, {}, 0, NO_LIMIT, Protos::Common::FindPattern::DIR);

   // Words and extensions.
   addRow("common word, one extension", common, { "mp3" });
   addRow("common word, two extensions", common, { "mkv", "avi" });

   // Words and sizes. The size of a directory is the one of its content.
   addRow("common word, size range", common, {}, 5 * MB, 10 * MB);
   addRow("common word, minimum size", common, {}, GB);
   addRow("common word, no file of this size", common, {}, 100 * GB, NO_LIMIT, Protos::Common::FindPattern::FILE, false); // All the entries with this word are filtered.

   // Words and all the filters.
   addRow("common word, all filters", common, { "mkv" }, 500 * MB, GB, Protos::Common::FindPattern::FILE);
   addRow("three words, all filters", threeWords, { "mkv" }, 500 * MB, GB, Protos::Common::FindPattern::FILE);

   // Without word.
   addRow("one extension", QString(), { "flac" });
   addRow("two extensions and size range", QString(), { "mkv", "avi" }, 500 * MB, GB);
   addRow("size range", QString(), {}, 5 * MB, 10 * MB);
}

/**
  * The duration of a search received from another peer, see 'NL::UDPListener'.
  */
void FindBenchmark::find()
{
   QFETCH(QString, words);
   QFETCH(QStringList, extensions);
   QFETCH(qint64, minFileSize);
   QFETCH(qint64, maxFileSize);
   QFETCH(int, category);
   QFETCH(bool, found);

   const auto search = [&] {
      return this->fileManager->find(words, extensions, minFileSize, maxFileSize, static_cast<Category>(category), MAX_NB_RESULT, MAX_SIZE, true);
   };

   const QList<Protos::Common::FindResult> results = search();
   int nbEntries = 0;
   for (const Protos::Common::FindResult& result : results)
      nbEntries += result.entries_size();
   QCOMPARE(nbEntries != 0, found);

   qsizetype nbMessages = 0;
   const double duration = BenchmarkUtils::measure([&] { nbMessages += search().size(); });
   QCOMPARE(nbMessages != 0, found);

   qInfo().noquote() << QString::number(duration / 1e3, 'f', 3) << "us," << nbEntries << "entries in" << results.size() << "messages";
}

void FindBenchmark::cleanupTestCase()
{
   delete this->fileManager;
   this->fileManager = nullptr;
}
