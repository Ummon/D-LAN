#include <QtTest>
#include <QTemporaryDir>
#include <QItemSelectionModel>
#include <QMimeData>
#include <QDataStream>
#include <limits>

#include <Common/Global.h>
#include <Common/RemoteCoreController/Builder.h>
#include <Downloads/DownloadsFlatModel.h>

namespace
{
   constexpr quint64 ETA_UNKNOWN = std::numeric_limits<quint64>::max();

   class Filter : public GUI::IFilter<GUI::DownloadFilterStatus>
   {
   public:
      QList<GUI::DownloadFilterStatus> filtered;
      QList<GUI::DownloadFilterStatus> getFilteredValues() const override { return filtered; }
   };

   class Model : public GUI::DownloadsFlatModel
   {
   public:
      using GUI::DownloadsFlatModel::DownloadsFlatModel;
      using GUI::DownloadsFlatModel::updateProgress;
      using GUI::DownloadsFlatModel::dropMimeData;
   };

   struct Fixture
   {
      QSharedPointer<RCC::ICoreConnection> connection = RCC::Builder::newCoreConnection(1000);
      GUI::PeerListModel peers { connection };
      GUI::SharedEntryListModel shares;
      Filter filter;
      Model model { connection, peers, shares, filter };
      Protos::GUI::State state;

      Fixture()
      {
         auto* download = this->state.add_downloads();
         download->set_id(1);
         download->set_status(Protos::Common::DOWNLOADING);
         auto* entry = download->mutable_local_entry();
         entry->set_name("test.bin");
         entry->set_path("/");
         entry->set_type(Protos::Common::Entry::FILE);
         entry->set_size(10000);
      }

      quint64 update(quint32 rate)
      {
         this->state.mutable_stats()->set_download_rate(rate);
         this->model.updateProgress(this->state);
         return this->model.getEta();
      }

      void setDownloads(const QList<quint64>& ids)
      {
         this->state.clear_downloads();
         for (const auto id : ids)
         {
            auto* download = this->state.add_downloads();
            download->set_id(id);
            download->set_status(Protos::Common::DOWNLOADING);
            auto* entry = download->mutable_local_entry();
            entry->set_name(QString("file-%1.bin").arg(id).toStdString());
            entry->set_type(Protos::Common::Entry::FILE);
            entry->set_size(10000);
         }
      }

      QList<quint64> ids() const
      {
         QList<quint64> result;
         for (int row = 0; row < this->model.rowCount(); ++row)
            result.append(this->model.getDownloadIDs(this->model.index(row, 0)).first());
         return result;
      }
   };
}

class TestsDownloadsFlatModel : public QObject
{
   Q_OBJECT

private slots:
   void emptyModelReleasesCapacity_data()
   {
      QTest::addColumn<int>("removal");
      QTest::newRow("queue cleared") << 0;
      QTest::newRow("all rows filtered") << 1;
      QTest::newRow("all rows dragged") << 2;
   }

   void emptyModelReleasesCapacity()
   {
      QFETCH(int, removal);
      Fixture f;
      QList<quint64> ids;
      for (int i = 0; i < 1000; ++i)
         ids.append(i + 1);

      for (int cycle = 0; cycle < 3; ++cycle)
      {
         f.filter.filtered.clear();
         f.setDownloads(ids);
         f.model.updateDownloads(f.state);
         QCOMPARE(f.ids(), ids);
         QVERIFY(f.model.downloads.capacity() >= ids.size());
         QPersistentModelIndex index(f.model.index(0, 0));
         QItemSelectionModel selection(&f.model);
         selection.select(index, QItemSelectionModel::Select | QItemSelectionModel::Rows);
         QSignalSpy removed(&f.model, &QAbstractItemModel::rowsRemoved);

         if (removal == 2)
         {
            // Encode row indices without asking for decorations (this test uses QCoreApplication).
            QByteArray encoded;
            QDataStream stream(&encoded, QIODevice::WriteOnly);
            for (int row = 0; row < f.model.rowCount(); ++row)
               stream << row << 0 << QMap<int, QVariant>();
            QMimeData mime;
            mime.setData(f.model.mimeTypes().first(), encoded);
            QVERIFY(f.model.dropMimeData(&mime, Qt::MoveAction, f.model.rowCount(), 0, QModelIndex()));
         }
         else
         {
            if (removal == 0)
               f.state.clear_downloads();
            else
               f.filter.filtered.append(GUI::STATUS_DOWNLOADING);
            f.model.updateDownloads(f.state);
         }

         QCOMPARE(f.model.rowCount(), 0);
         QCOMPARE(f.model.downloads.capacity(), qsizetype(0));
         QVERIFY(!index.isValid());
         QVERIFY(selection.selectedRows().isEmpty());
         QCOMPARE(removed.size(), 1);
         QCOMPARE(removed[0][1].toInt(), 0);
         QCOMPARE(removed[0][2].toInt(), ids.size() - 1);
         f.state.clear_downloads();
         f.model.updateDownloads(f.state);
         QCOMPARE(f.model.downloads.capacity(), qsizetype(0));
         QCOMPARE(removed.size(), 1);
      }
   }

   void nonemptyModelKeepsCapacity()
   {
      Fixture f;
      QList<quint64> ids;
      for (int i = 0; i < 1000; ++i)
         ids.append(i + 1);
      f.setDownloads(ids);
      f.model.updateDownloads(f.state);
      const auto capacity = f.model.downloads.capacity();
      QPersistentModelIndex first(f.model.index(0, 0));
      f.setDownloads({1});
      f.model.updateDownloads(f.state);
      QCOMPARE(f.ids(), QList<quint64>{1});
      QCOMPARE(f.model.downloads.capacity(), capacity);
      QVERIFY(first.isValid());
      QCOMPARE(first.row(), 0);
   }

   void insertsWholeRanges_data()
   {
      QTest::addColumn<int>("oldCount");
      QTest::addColumn<int>("position");
      QTest::newRow("empty") << 0 << 0;
      QTest::newRow("front") << 20 << 0;
      QTest::newRow("middle") << 20 << 10;
      QTest::newRow("end") << 20 << 20;
   }

   void insertsWholeRanges()
   {
      QFETCH(int, oldCount);
      QFETCH(int, position);
      Fixture f;
      QList<quint64> before;
      for (int i = 0; i < oldCount; ++i)
         before.append(i + 1);
      f.setDownloads(before);
      f.model.updateDownloads(f.state);

      QList<QPersistentModelIndex> persistent;
      QItemSelectionModel selection(&f.model);
      for (int row = 0; row < oldCount; ++row)
      {
         persistent.append(f.model.index(row, 0));
         selection.select(f.model.index(row, 0), QItemSelectionModel::Select | QItemSelectionModel::Rows);
      }

      constexpr int count = 1000;
      QList<quint64> expected = before.first(position);
      for (int i = 0; i < count; ++i)
         expected.append(100 + i);
      expected.append(before.sliced(position));
      f.setDownloads(expected);
      QSignalSpy inserted(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy removed(&f.model, &QAbstractItemModel::rowsRemoved);
      QSignalSpy changed(&f.model, &QAbstractItemModel::dataChanged);
      QSignalSpy reset(&f.model, &QAbstractItemModel::modelReset);
      connect(&f.model, &QAbstractItemModel::rowsAboutToBeInserted, this,
         [&](const QModelIndex& parent, int first, int last) {
            QVERIFY(!parent.isValid());
            QCOMPARE(first, position);
            QCOMPARE(last, position + count - 1);
            QCOMPARE(f.ids(), before);
         });
      connect(&f.model, &QAbstractItemModel::rowsInserted, this,
         [&](const QModelIndex&, int, int) {
            QCOMPARE(f.ids(), expected);
            for (int row = 0; row < expected.size(); ++row)
               QCOMPARE(f.model.index(row, 0).data().toString(), QString("file-%1.bin").arg(expected[row]));
         });

      f.model.updateDownloads(f.state);
      QCOMPARE(f.ids(), expected);
      QCOMPARE(inserted.count(), 1);
      QCOMPARE(removed.count(), 0);
      QCOMPARE(changed.count(), 0);
      QCOMPARE(reset.count(), 0);
      for (int row = 0; row < oldCount; ++row)
      {
         QVERIFY(persistent[row].isValid());
         QCOMPARE(persistent[row].row(), row < position ? row : row + count);
         QCOMPARE(f.model.getDownloadIDs(persistent[row]).first(), before[row]);
         QVERIFY(selection.isSelected(persistent[row]));
      }
      QCOMPARE(selection.selectedRows().size(), oldCount);
      f.model.updateDownloads(f.state);
      QCOMPARE(inserted.count(), 1);
      QCOMPARE(changed.count(), 0);
   }

   void mixedRangesRespectFiltersAndUpdates()
   {
      Fixture f;
      f.setDownloads({ 10, 20, 30, 40 });
      f.model.updateDownloads(f.state);
      QPersistentModelIndex retained = f.model.index(2, 0);
      QPersistentModelIndex deleted = f.model.index(1, 0);
      f.setDownloads({ 1, 2, 10, 11, 12, 30, 31, 32 });
      f.state.mutable_downloads(2)->mutable_local_entry()->set_name("renamed.bin");
      f.state.mutable_downloads(3)->set_status(Protos::Common::PAUSED);
      f.filter.filtered = { GUI::STATUS_INACTIVE };
      QSignalSpy inserted(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy removed(&f.model, &QAbstractItemModel::rowsRemoved);
      QSignalSpy changed(&f.model, &QAbstractItemModel::dataChanged);
      f.model.updateDownloads(f.state);
      QCOMPARE(f.ids(), QList<quint64>({ 1, 2, 10, 12, 30, 31, 32 }));
      QCOMPARE(inserted.count(), 3);
      const QList<QPair<int, int>> ranges { { 0, 1 }, { 3, 3 }, { 5, 6 } };
      for (int i = 0; i < ranges.size(); ++i)
      {
         QCOMPARE(inserted[i][1].toInt(), ranges[i].first);
         QCOMPARE(inserted[i][2].toInt(), ranges[i].second);
      }
      QCOMPARE(removed.count(), 2);
      QCOMPARE(changed.count(), 1);
      QCOMPARE(changed[0][0].value<QModelIndex>().row(), 2);
      QCOMPARE(changed[0][1].value<QModelIndex>().row(), 2);
      QCOMPARE(f.model.index(2, 0).data().toString(), QString("renamed.bin"));
      QVERIFY(!deleted.isValid());
      QVERIFY(retained.isValid());
      QCOMPARE(retained.row(), 4);
      QCOMPARE(f.model.getDownloadIDs(retained).first(), quint64(30));

      f.filter.filtered.clear();
      f.model.updateDownloads(f.state);
      QCOMPARE(f.ids(), QList<quint64>({ 1, 2, 10, 11, 12, 30, 31, 32 }));
      QCOMPARE(inserted.count(), 4);
      QCOMPARE(inserted.last()[1].toInt(), 3);
      QCOMPARE(inserted.last()[2].toInt(), 3);
      QCOMPARE(retained.row(), 5);
      QVERIFY(f.model.isDownloadPaused(f.model.index(3, 0)));
   }

   void reorderingThenAppendingAndClearing()
   {
      Fixture f;
      f.setDownloads({ 1, 2, 3 });
      f.model.updateDownloads(f.state);
      f.setDownloads({ 3, 4, 5, 1, 2, 6, 7 });
      QSignalSpy inserted(&f.model, &QAbstractItemModel::rowsInserted);
      QSignalSpy changed(&f.model, &QAbstractItemModel::dataChanged);
      f.model.updateDownloads(f.state);
      QCOMPARE(f.ids(), QList<quint64>({ 3, 4, 5, 1, 2, 6, 7 }));
      QCOMPARE(inserted.count(), 1);
      QCOMPARE(inserted[0][1].toInt(), 3);
      QCOMPARE(inserted[0][2].toInt(), 6);
      QCOMPARE(changed.count(), 1);
      QCOMPARE(changed[0][0].value<QModelIndex>().row(), 0);
      QCOMPARE(changed[0][1].value<QModelIndex>().row(), 2);
      f.model.updateDownloads(f.state);
      QCOMPARE(inserted.count(), 1);
      QCOMPARE(changed.count(), 1);

      f.state.clear_downloads();
      QSignalSpy removed(&f.model, &QAbstractItemModel::rowsRemoved);
      f.model.updateDownloads(f.state);
      QCOMPARE(f.model.rowCount(), 0);
      QCOMPARE(removed.count(), 1);
      QCOMPARE(removed[0][1].toInt(), 0);
      QCOMPARE(removed[0][2].toInt(), 6);
   }

   void startupAndRollingAverage()
   {
      Fixture f;
      for (int i = 0; i < 9; ++i)
         QCOMPARE(f.update(1000), ETA_UNKNOWN);
      // Unused slots must not dilute the average while the window fills.
      for (int i = 0; i < 11; ++i)
         QCOMPARE(f.update(1000), quint64(10));
      QCOMPARE(f.update(2000), quint64(9));
      for (int i = 0; i < 19; ++i)
         f.update(2000);
      // The divisor stays at twenty after filling and wrapping the buffer.
      for (int i = 0; i < 25; ++i)
         QCOMPARE(f.update(2000), quint64(5));
   }

   void startupIncludesActualZeroSamples()
   {
      Fixture f;
      for (int i = 0; i < 5; ++i)
         QCOMPARE(f.update(0), ETA_UNKNOWN);
      for (int i = 0; i < 9; ++i)
         QCOMPARE(f.update(1000), ETA_UNKNOWN);
      // Ten positive samples and five actual zeros: 10000 / 15 = 666 B/s.
      QCOMPARE(f.update(1000), quint64(15));
   }

   void etaRequiresTenNonzeroSamplesInWindow()
   {
      Fixture f;
      for (int i = 0; i < 20; ++i)
         f.update(1000);
      for (int i = 0; i < 9; ++i)
         QVERIFY(f.update(0) != ETA_UNKNOWN);
      QCOMPARE(f.update(0), quint64(20));
      QCOMPARE(f.update(0), ETA_UNKNOWN);
      for (int i = 0; i < 9; ++i)
         QCOMPARE(f.update(1000), ETA_UNKNOWN);
      QCOMPARE(f.update(1000), quint64(20));
   }

   void slowRecoveryDoesNotDivideByZero()
   {
      Fixture f;
      for (int i = 0; i < 20; ++i)
         QCOMPARE(f.update(0), ETA_UNKNOWN);
      // Fewer than twenty bytes across twenty samples round down to zero.
      for (int i = 0; i < 19; ++i)
         QCOMPARE(f.update(1), ETA_UNKNOWN);
      QCOMPARE(f.update(1), quint64(10000));
   }
};

int main(int argc, char** argv)
{
   QCoreApplication app(argc, argv);
   QTemporaryDir data;
   if (!data.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, data.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, data.path());
   TestsDownloadsFlatModel tests;
   return QTest::qExec(&tests, argc, argv);
}
#include "TestsDownloadsFlatModel.moc"
