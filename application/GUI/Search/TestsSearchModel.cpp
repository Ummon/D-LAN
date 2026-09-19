#include <QtTest>
#include <QAbstractItemModelTester>
#include <QSignalSpy>

#include <limits>

#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/RemoteCoreController/Builder.h>
#include <Common/Settings.h>
#include <Search/SearchModel.h>

bool entryLessThan(
   const Protos::Common::Entry& e1, int level1, const QString& peerNick1,
   const Protos::Common::Entry& e2, int level2, const QString& peerNick2,
   GUI::SearchModel::Column column, Qt::SortOrder order
);

namespace
{
   class Model : public GUI::SearchModel
   {
   public:
      using GUI::SearchModel::SearchModel;
      using GUI::SearchModel::resultFromFindResult;
      using GUI::SearchModel::sort;
   };

   struct Fixture
   {
      QSharedPointer<RCC::ICoreConnection> connection = RCC::Builder::newCoreConnection(1000);
      GUI::PeerListModel peers { connection };
      GUI::SharedEntryListModel shares;
      Model model { connection, peers, shares };
   };

   Protos::Common::FindResult result(const char* name, quint64 size, const QList<Common::Hash>& hashes, int level = 0)
   {
      Protos::Common::FindResult result;
      const auto peer = Common::Hash::rand();
      result.mutable_peer_id()->set_hash(peer.getData(), Common::Hash::HASH_SIZE);
      auto entry = result.add_entries();
      entry->set_level(level);
      entry->mutable_entry()->set_type(Protos::Common::Entry::FILE);
      entry->mutable_entry()->set_name(name);
      entry->mutable_entry()->set_path("/");
      entry->mutable_entry()->set_size(size);
      for (const auto& hash : hashes)
         entry->mutable_entry()->add_chunks()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      return result;
   }
}

class TestsSearchModel : public QObject
{
   Q_OBJECT
private slots:
   void entryOrdering_data()
   {
      QTest::addColumn<int>("column");
      QTest::addColumn<QList<int>>("expected");
      // Entries 0..4 each lower one key (name, directory, level, peer, size).
      // Entries 5 and 6 have equivalent keys, including case-insensitive name/path.
      QTest::newRow("name") << int(Model::NAME) << QList<int>{0, 2, 1, 3, 4, 5, 6};
      QTest::newRow("directory") << int(Model::DIRECTORY) << QList<int>{1, 2, 0, 3, 4, 5, 6};
      QTest::newRow("relevance") << int(Model::RELEVANCE) << QList<int>{2, 1, 0, 3, 4, 5, 6};
      QTest::newRow("peer") << int(Model::PEER) << QList<int>{3, 2, 1, 0, 4, 5, 6};
      QTest::newRow("size") << int(Model::SIZE) << QList<int>{4, 2, 1, 0, 3, 5, 6};
   }

   void entryOrdering()
   {
      QFETCH(int, column);
      QFETCH(QList<int>, expected);
      struct Entry
      {
         Protos::Common::Entry item;
         int level = std::numeric_limits<int>::max();
         QString peer = "b";
      };
      QList<Entry> entries;
      for (int i = 0; i < 7; ++i)
      {
         Entry entry;
         entry.item.set_type(Protos::Common::Entry::FILE);
         entry.item.set_name("B");
         entry.item.set_path("/b/");
         entry.item.mutable_shared_entry()->set_path("/share/");
         entry.item.set_size(std::numeric_limits<quint64>::max());
         entries.append(entry);
      }
      entries[0].item.set_name("A");
      entries[1].item.set_path("/a/");
      entries[2].level = 0;
      entries[3].peer = "A";
      entries[4].item.set_size(0);
      entries[6].item.set_name("b");
      entries[6].item.set_path("/B/");

      for (const auto order : { Qt::AscendingOrder, Qt::DescendingOrder })
      {
         for (int i = 0; i < expected.size(); ++i)
         {
            for (int j = 0; j < expected.size(); ++j)
            {
               const auto& left = entries[expected[i]];
               const auto& right = entries[expected[j]];
               const bool equivalent = expected[i] >= 5 && expected[j] >= 5;
               QCOMPARE(
                  entryLessThan(left.item, left.level, left.peer, right.item, right.level, right.peer,
                     static_cast<Model::Column>(column), order),
                  !equivalent && (order == Qt::AscendingOrder ? i < j : i > j)
               );
            }
         }
      }
   }

   void emptySearch()
   {
      Fixture f;
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QCOMPARE(f.model.rowCount(), 0);
      f.model.resultFromFindResult({});
      QCOMPARE(f.model.rowCount(), 0);
   }

   void groupsMatchingSearchHashes_data()
   {
      QTest::addColumn<quint64>("size");
      QTest::addColumn<int>("hashCount");
      QTest::newRow("complete-single-chunk") << quint64(123) << 1;
      QTest::newRow("complete-eight-chunks") << quint64(8) * Common::Constants::CHUNK_SIZE << 8;
      // FileManager::find sends at most eight known hashes per entry.
      QTest::newRow("truncated-search-hashes") << quint64(9) * Common::Constants::CHUNK_SIZE << 8;
   }

   void groupsMatchingSearchHashes()
   {
      QFETCH(quint64, size);
      QFETCH(int, hashCount);
      Fixture f;
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      QList<Common::Hash> hashes;
      for (int i = 0; i < hashCount; ++i)
         hashes.append(Common::Hash::rand());
      const auto original = result("original", size, hashes, 2);
      const auto copy = result("copy", size, hashes, 1);
      f.model.resultFromFindResult(original);
      QPersistentModelIndex group = f.model.index(0, 0);
      QSignalSpy insertions(&f.model, &QAbstractItemModel::rowsInserted);
      f.model.resultFromFindResult(copy);

      QCOMPARE(f.model.rowCount(), 1);
      QCOMPARE(f.model.rowCount(group), 2);
      QCOMPARE(f.model.getNbFiles(), 1);
      QVERIFY(Model::isNonTerminalFile(group));
      QCOMPARE(group.data().toString(), QString("copy"));
      QCOMPARE(insertions.size(), 2);
      for (const auto& insertion : insertions)
         QCOMPARE(insertion.at(0).value<QModelIndex>(), QModelIndex(group));
      QCOMPARE(f.model.getPeerID(f.model.index(0, 0, group)), Common::Hash(copy.peer_id().hash()));
      QCOMPARE(f.model.getPeerID(f.model.index(1, 0, group)), Common::Hash(original.peer_id().hash()));

      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      f.model.resultFromFindResult(result("third", size, hashes, 2));
      QCOMPARE(f.model.rowCount(), 1);
      QCOMPARE(f.model.rowCount(group), 3);
      QCOMPARE(changes.size(), 1);
      QCOMPARE(changes.at(0).at(0).value<QModelIndex>(), QModelIndex(group));
   }

   void retainsCandidatesSharingFirstChunk()
   {
      Fixture f;
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      const auto first = Common::Hash::rand();
      const QList<Common::Hash> hashesA { first, Common::Hash::rand() };
      const QList<Common::Hash> hashesB { first, Common::Hash::rand() };
      const quint64 size = 2 * Common::Constants::CHUNK_SIZE;
      f.model.resultFromFindResult(result("a", size, hashesA));
      QPersistentModelIndex groupA = f.model.index(0, 0);
      f.model.resultFromFindResult(result("b", size, hashesB));
      QPersistentModelIndex groupB = f.model.index(1, 0);
      f.model.sort(Model::NAME, Qt::DescendingOrder);
      QCOMPARE(groupA.row(), 1);
      QSignalSpy insertions(&f.model, &QAbstractItemModel::rowsInserted);
      f.model.resultFromFindResult(result("copy-a", size, hashesA));
      QCOMPARE(f.model.rowCount(), 2);
      QCOMPARE(f.model.rowCount(groupA), 2);
      QCOMPARE(f.model.rowCount(groupB), 0);
      for (const auto& insertion : insertions)
         QCOMPARE(insertion.at(0).value<QModelIndex>(), QModelIndex(groupA));
      f.model.resultFromFindResult(result("copy-b", size, hashesB));
      QCOMPARE(f.model.rowCount(), 2);
      QCOMPARE(f.model.rowCount(groupB), 2);
   }

   void rejectsNonMatchingEntries_data()
   {
      QTest::addColumn<QString>("difference");
      for (const char* difference : { "size", "later-hash", "hash-count", "unknown-hash", "no-hashes", "directory" })
         QTest::newRow(difference) << QString(difference);
   }

   void rejectsNonMatchingEntries()
   {
      QFETCH(QString, difference);
      Fixture f;
      const quint64 size = 2 * Common::Constants::CHUNK_SIZE;
      auto original = result("a", size, { Common::Hash::rand(), Common::Hash::rand() });
      if (difference == "unknown-hash")
         original.mutable_entries(0)->mutable_entry()->mutable_chunks(1)->clear_hash();
      else if (difference == "no-hashes")
         original.mutable_entries(0)->mutable_entry()->clear_chunks();
      auto other = original;
      auto entry = other.mutable_entries(0)->mutable_entry();
      entry->set_name("b");
      if (difference == "size")
         entry->set_size(size - 1);
      else if (difference == "later-hash")
         entry->mutable_chunks(1)->set_hash(Common::Hash::rand().getData(), Common::Hash::HASH_SIZE);
      else if (difference == "hash-count")
         entry->mutable_chunks()->RemoveLast();
      else if (difference == "directory")
      {
         entry->set_type(Protos::Common::Entry::DIR);
         entry->set_is_empty(true);
      }
      f.model.resultFromFindResult(original);
      f.model.resultFromFindResult(other);
      QCOMPARE(f.model.rowCount(), 2);
      QCOMPARE(f.model.rowCount(f.model.index(0, 0)), 0);
      QCOMPARE(f.model.rowCount(f.model.index(1, 0)), 0);
   }
};

int main(int argc, char** argv)
{
   qInstallMessageHandler(nullptr);
   QApplication app(argc, argv);
   QTemporaryDir data;
   if (!data.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, data.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, data.path());
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   TestsSearchModel tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsSearchModel.moc"
