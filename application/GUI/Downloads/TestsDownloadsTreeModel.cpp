#include <QtTest>
#include <QAbstractItemModelTester>
#include <QPersistentModelIndex>
#include <QSignalSpy>
#include <Common/RemoteCoreController/Builder.h>
#include <Downloads/DownloadsTreeModel.h>

namespace
{
   class Filter : public GUI::IFilter<GUI::DownloadFilterStatus>
   {
   public:
      QList<GUI::DownloadFilterStatus> values;
      QList<GUI::DownloadFilterStatus> getFilteredValues() const override { return this->values; }
   };

   class Model : public GUI::DownloadsTreeModel
   {
   public:
      using GUI::DownloadsTreeModel::DownloadsTreeModel;
      using GUI::DownloadsTreeModel::onNewState;
   };

   struct Fixture
   {
      QSharedPointer<RCC::ICoreConnection> connection = RCC::Builder::newCoreConnection(1000);
      GUI::PeerListModel peers { connection };
      GUI::SharedEntryListModel shares;
      Filter filter;
      Model model { connection, peers, shares, filter };
   };

   void addDownload(Protos::GUI::State& state, quint64 id, const QString& name, const char* path = "/")
   {
      auto download = state.add_downloads();
      download->set_id(id);
      download->set_status(Protos::Common::QUEUED);
      auto entry = download->mutable_local_entry();
      entry->set_name(name.toStdString());
      entry->set_path(path);
      entry->set_type(Protos::Common::Entry::FILE);
      entry->set_size(100);
   }

   QStringList names(const Model& model, const QModelIndex& parent = {})
   {
      QStringList result;
      for (int row = 0; row < model.rowCount(parent); ++row)
         result.append(model.index(row, 0, parent).data().toString());
      return result;
   }
}

class TestsDownloadsTreeModel : public QObject
{
   Q_OBJECT
private slots:
   void largeQueue_data()
   {
      QTest::addColumn<bool>("grouped");
      QTest::newRow("top-level") << false;
      QTest::newRow("one-directory") << true;
   }

   void largeQueue()
   {
      QFETCH(bool, grouped);
      const int configuredCount = qEnvironmentVariableIntValue("DLAN_TEST_DOWNLOAD_COUNT");
      const int count = configuredCount > 0 ? configuredCount : 2000;
      Fixture f;
      Protos::GUI::State state;
      for (int i = 0; i < count; ++i)
         addDownload(state, i + 1, QString("file-%1.bin").arg(i, 6, 10, QChar('0')), grouped ? "/directory/" : "/");
      QElapsedTimer timer;
      timer.start();
      f.model.onNewState(state);
      const double initialMs = timer.nsecsElapsed() / 1e6;
      const QModelIndex parent = grouped ? f.model.index(0, 0) : QModelIndex();
      QCOMPARE(f.model.rowCount(parent), count);
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      QSignalSpy moves(&f.model, &QAbstractItemModel::rowsMoved);
      timer.restart();
      f.model.onNewState(state);
      const double unchangedMs = timer.nsecsElapsed() / 1e6;
      QCOMPARE(changes.size(), 0);
      QCOMPARE(moves.size(), 0);
      for (auto& download : *state.mutable_downloads())
      {
         download.set_downloaded_bytes(50);
         download.set_status(Protos::Common::DOWNLOADING);
      }
      timer.restart();
      f.model.onNewState(state);
      const double progressMs = timer.nsecsElapsed() / 1e6;
      qInfo("%d downloads: initial %.3f ms, unchanged %.3f ms, progress %.3f ms, %lld dataChanged signals",
         count, initialMs, unchangedMs, progressMs, static_cast<long long>(changes.size()));
      QCOMPARE(changes.size(), grouped ? 2 : 1);
      for (int row : { 0, count / 2, count - 1 })
      {
         const auto index = f.model.index(row, GUI::DownloadsModel::PROGRESS, parent);
         const auto progress = index.data().value<GUI::Progress>();
         QCOMPARE(progress.progress, quint32(5000));
         QCOMPARE(f.model.parent(index), parent);
         QCOMPARE(f.model.getDownloadIDs(index), (QList<quint64> { quint64(row + 1) }));
      }
      if (grouped)
         QCOMPARE(f.model.index(0, GUI::DownloadsModel::PROGRESS).data().value<GUI::Progress>().progress, quint32(5000));
      changes.clear();
      f.model.onNewState(state);
      QCOMPARE(changes.size(), 0);
   }

   void structuralChangesPreserveIndexes()
   {
      Fixture f;
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      Protos::GUI::State state;
      addDownload(state, 1, "z", "/A/");
      addDownload(state, 2, "a", "/A/");
      addDownload(state, 3, "b", "/B/deep/");
      addDownload(state, 4, "loose");
      f.model.onNewState(state);
      QCOMPARE(names(f.model), (QStringList { "A", "B", "loose" }));
      QPersistentModelIndex aDirectory = f.model.index(0, 0);
      QPersistentModelIndex bDirectory = f.model.index(1, 0);
      QPersistentModelIndex aFile = f.model.index(0, 0, aDirectory);
      QPersistentModelIndex removedFile = f.model.index(1, 0, aDirectory);
      QPersistentModelIndex bFile = f.model.index(0, 0, f.model.index(0, 0, bDirectory));
      QPersistentModelIndex loose = f.model.index(2, 0);
      QCOMPARE(names(f.model, aDirectory), (QStringList { "a", "z" }));

      state.clear_downloads();
      addDownload(state, 4, "loose");
      addDownload(state, 3, "b", "/B/deep/");
      addDownload(state, 2, "a", "/A/");
      addDownload(state, 5, "m", "/A/");
      addDownload(state, 6, "n", "/A/deep/");
      state.mutable_downloads(0)->set_downloaded_bytes(20);
      state.mutable_downloads(0)->set_status(Protos::Common::DOWNLOADING);
      state.mutable_downloads(1)->set_downloaded_bytes(50);
      state.mutable_downloads(1)->set_status(Protos::Common::DOWNLOADING);
      state.mutable_downloads(2)->set_downloaded_bytes(100);
      state.mutable_downloads(2)->set_status(Protos::Common::COMPLETE);
      state.mutable_downloads(3)->set_status(Protos::Common::PAUSED);
      QSignalSpy changes(&f.model, &QAbstractItemModel::dataChanged);
      f.model.onNewState(state);
      QCOMPARE(names(f.model), (QStringList { "loose", "B", "A" }));
      QCOMPARE(names(f.model, aDirectory), (QStringList { "deep", "a", "m" }));
      QVERIFY(!removedFile.isValid());
      QCOMPARE(loose.row(), 0);
      QCOMPARE(aDirectory.row(), 2);
      QCOMPARE(aFile.row(), 1);
      QCOMPARE(f.model.parent(aFile), QModelIndex(aDirectory));
      QCOMPARE(f.model.parent(f.model.parent(bFile)), QModelIndex(bDirectory));
      QCOMPARE(f.model.getDownloadIDs(bFile), (QList<quint64> { 3 }));
      const auto aProgress = f.model.index(2, GUI::DownloadsModel::PROGRESS).data().value<GUI::Progress>();
      QCOMPARE(aProgress.progress, quint32(3333));
      QCOMPARE(aProgress.status, Protos::Common::PAUSED);
      for (const auto& signal : changes)
      {
         const auto first = signal[0].value<QModelIndex>();
         const auto last = signal[1].value<QModelIndex>();
         QCOMPARE(first.parent(), last.parent());
         QVERIFY(first.row() <= last.row());
         QVERIFY(last.row() < f.model.rowCount(last.parent()));
      }

      f.filter.values = { GUI::STATUS_COMPLETE };
      f.model.onNewState(state);
      QVERIFY(!aFile.isValid());
      QCOMPARE(names(f.model, aDirectory), (QStringList { "deep", "m" }));
      QCOMPARE(f.model.index(2, GUI::DownloadsModel::PROGRESS).data().value<GUI::Progress>().progress, quint32(0));
      f.filter.values.clear();
      f.model.onNewState(state);
      QCOMPARE(names(f.model, aDirectory), (QStringList { "deep", "a", "m" }));

      state.clear_downloads();
      f.model.onNewState(state);
      QCOMPARE(f.model.rowCount(), 0);
      QVERIFY(!aDirectory.isValid());
      QVERIFY(!bFile.isValid());
      QVERIFY(!loose.isValid());
      addDownload(state, 7, "new", "/fresh/sub/");
      f.model.onNewState(state);
      QCOMPARE(names(f.model), (QStringList { "fresh" }));
      QCOMPARE(f.model.getDownloadIDs(f.model.index(0, 0)), (QList<quint64> { 7 }));
   }

   void newDescendantOrdersItsExistingDirectory()
   {
      Fixture f;
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      Protos::GUI::State state;
      addDownload(state, 1, "a", "/A/");
      addDownload(state, 2, "b", "/B/");
      addDownload(state, 3, "c", "/C/");
      f.model.onNewState(state);
      QPersistentModelIndex b = f.model.index(1, 0);
      state.clear_downloads();
      addDownload(state, 4, "new", "/B/");
      addDownload(state, 2, "b", "/B/");
      addDownload(state, 1, "a", "/A/");
      addDownload(state, 3, "c", "/C/");
      f.model.onNewState(state);
      QCOMPARE(names(f.model), (QStringList { "B", "A", "C" }));
      QCOMPARE(b.row(), 0);
      QCOMPARE(names(f.model, b), (QStringList { "b", "new" }));
   }

   void sortedInsertionAndStatusChanges()
   {
      Fixture f;
      QAbstractItemModelTester tester(&f.model, QAbstractItemModelTester::FailureReportingMode::QtTest);
      Protos::GUI::State state;
      const QStringList input { "Zulu", "Alpha", "ALPHA", "a", "0" };
      for (int i = 0; i < input.size(); ++i)
         addDownload(state, i + 1, input[i], "/folder/");
      addDownload(state, 6, "child", "/folder/sub/");
      f.model.onNewState(state);
      const auto folder = f.model.index(0, 0);
      QCOMPARE(names(f.model, folder), (QStringList { "sub", "0", "a", "Alpha", "ALPHA", "Zulu" }));
      state.mutable_downloads(0)->set_status(Protos::Common::NO_SOURCE);
      f.model.onNewState(state);
      QCOMPARE(f.model.index(0, GUI::DownloadsModel::PROGRESS).data().value<GUI::Progress>().status, Protos::Common::NO_SOURCE);
      state.mutable_downloads(0)->set_status(Protos::Common::PAUSED);
      f.model.onNewState(state);
      QCOMPARE(f.model.index(0, GUI::DownloadsModel::PROGRESS).data().value<GUI::Progress>().status, Protos::Common::PAUSED);
      for (auto& download : *state.mutable_downloads())
      {
         download.set_status(Protos::Common::COMPLETE);
         download.set_downloaded_bytes(100);
      }
      f.model.onNewState(state);
      QVERIFY(f.model.isFileComplete(folder));
      const auto progress = f.model.index(0, GUI::DownloadsModel::PROGRESS).data().value<GUI::Progress>();
      QCOMPARE(progress.status, Protos::Common::COMPLETE);
      QCOMPARE(progress.progress, quint32(10000));
   }
};

QTEST_MAIN(TestsDownloadsTreeModel)
#include "TestsDownloadsTreeModel.moc"
