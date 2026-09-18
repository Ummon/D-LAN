#include <QtTest>
#include <QAbstractItemModelTester>
#include <QHeaderView>
#include <QTemporaryFile>
#include <QScrollBar>
#include <QTemporaryDir>
#include <QPushButton>
#include <QComboBox>
#include <QScopeGuard>

#include <TableLogModel.h>
#include <TableLogItemDelegate.h>
#include <LogTableView.h>
#include <MainWindow.h>
#include <Common/Global.h>

namespace
{
QByteArray line(const QByteArray& message, const QByteArray& module = "Module")
{
   return "2026-09-18 10:00:00.001 [Debug] {" + module + "} (main) : " + message + '\n';
}

// A large logical model lets the view tests detect accidental work on offscreen rows.
class LargeModel : public TableLogModel
{
public:
   mutable int requests = 0;
   QString message = "<pre>first\nsecond\nthird</pre>";
   int rows = 100000;
   int rowCount(const QModelIndex& parent = {}) const override { return parent.isValid() ? 0 : rows; }
   QVariant data(const QModelIndex& index, int role) const override
   {
      ++requests;
      if (role == Qt::DisplayRole)
         return index.column() == MESSAGE ? this->message : QString("metadata");
      return {};
   }
   void replaceMessage(const QString& message)
   {
      this->beginResetModel();
      this->message = message;
      this->endResetModel();
   }
   void appendRow()
   {
      this->beginInsertRows({}, this->rows, this->rows);
      ++this->rows;
      this->endInsertRows();
   }
};
}

class TestsLogViewer : public QObject
{
   Q_OBJECT
private slots:
   void incrementalLoading();
   void switchingFilesCancelsBatches();
   void filteringAndLiveSearch();
   void visibleRowSizing();
   void documentCacheTracksContentAndFont();
   void refreshButtonKeepsExistingRows();
};

void TestsLogViewer::incrementalLoading()
{
   QTemporaryFile file;
   QVERIFY(file.open());
   for (int i = 0; i < 1000; ++i)
      file.write(line(QByteArray::number(i) + " caf\xc3\xa9"));
   file.flush();
   file.seek(0);
   TableLogModel model;
   QAbstractItemModelTester tester(&model, QAbstractItemModelTester::FailureReportingMode::QtTest);
   QSignalSpy finished(&model, &TableLogModel::loadingFinished);
   QSignalSpy batches(&model, &TableLogModel::newLogEntries);
   int heartbeats = 0;
   QTimer heartbeat;
   connect(&heartbeat, &QTimer::timeout, [&] { ++heartbeats; });
   heartbeat.start(1);
   model.setDataSource(&file);
   QCOMPARE(model.rowCount(), 0);
   QTRY_VERIFY_WITH_TIMEOUT(!finished.isEmpty(), 10000);
   QCOMPARE(model.rowCount(), 1000);
   QVERIFY(batches.size() > 1);
   QVERIFY(heartbeats > 1);
   for (const auto& batch : batches)
      QVERIFY(batch[0].toInt() <= 256);
   for (int row = 0; row < 1000; ++row)
      QCOMPARE(model.data(model.index(row, TableLogModel::MESSAGE)).toString(), QString::number(row) + QString::fromUtf8(" caf\xc3\xa9"));
}

void TestsLogViewer::switchingFilesCancelsBatches()
{
   QTemporaryFile first;
   QTemporaryFile second;
   QVERIFY(first.open());
   QVERIFY(second.open());
   for (int i = 0; i < 1000; ++i)
      first.write(line("old"));
   first.flush();
   first.seek(0);
   second.write(line("new"));
   second.flush();
   second.seek(0);
   TableLogModel model;
   model.setDataSource(&first);
   QTRY_VERIFY(model.rowCount() > 0);
   QVERIFY(model.rowCount() < 1000);
   model.setDataSource(&second);
   first.close();
   QTRY_COMPARE(model.rowCount(), 1);
   QTest::qWait(30);
   QCOMPARE(model.data(model.index(0, TableLogModel::MESSAGE)).toString(), QString("new"));
   model.removeDataSource();
   second.close();
   QTest::qWait(20);
   QCOMPARE(model.rowCount(), 0);
}

void TestsLogViewer::filteringAndLiveSearch()
{
   QTemporaryFile file;
   QVERIFY(file.open());
   file.write(line("needle", "A") + line("other", "B"));
   file.flush();
   file.seek(0);
   TableLogModel model;
   QAbstractItemModelTester tester(&model, QAbstractItemModelTester::FailureReportingMode::QtTest);
   model.search("NEEDLE");
   model.setDataSource(&file);
   QTRY_COMPARE(model.rowCount(), 2);
   QCOMPARE(model.currentNbFoundItems(), 1);
   model.setFilter(model.getSeverities(), {"B"}, model.getThreads());
   QCOMPARE(model.rowCount(), 1);
   QCOMPARE(model.currentNbFoundItems(), 0);

   QFile writer(file.fileName());
   QVERIFY(writer.open(QIODevice::Append));
   writer.write(line("needle again", "B"));
   writer.flush();
   model.setWatchingPause(false);
   QTRY_COMPARE_WITH_TIMEOUT(model.rowCount(), 2, 2000);
   QCOMPARE(model.currentNbFoundItems(), 1);
   QVERIFY(model.inSearchResult(model.index(1, TableLogModel::MESSAGE)));
   model.setWatchingPause(true);
   writer.write(line("paused", "B"));
   writer.flush();
   QTest::qWait(550);
   QCOMPARE(model.rowCount(), 2);
   model.setWatchingPause(false);
   QTRY_COMPARE_WITH_TIMEOUT(model.rowCount(), 3, 2000);
   model.setWatchingPause(true);
   model.setFilter({}, {}, {});
   QCOMPARE(model.rowCount(), 0);
   model.removeDataSource(); // Resetting a fully filtered model must also be valid.
   QCOMPARE(model.currentNbFoundItems(), 0);
   QVERIFY(model.getModules().isEmpty());
}

void TestsLogViewer::visibleRowSizing()
{
   LargeModel model;
   LogTableView view;
   view.resize(1000, 500);
   view.setModel(&model);
   view.show();
   QTest::qWait(30);
   const int height = view.verticalHeader()->defaultSectionSize();
   QCOMPARE(view.rowHeight(0), height);
   QVERIFY(model.requests < 5000);
   model.requests = 0;
   model.appendRow();
   view.scrollToBottom();
   QTest::qWait(30);
   QVERIFY(model.requests < 5000);
   view.scrollToTop();
   model.requests = 0;
   view.setShowMultipleLines(true);
   QTRY_VERIFY(view.rowHeight(0) > height);
   QCOMPARE(view.rowHeight(50000), height);
   QVERIFY(model.requests < 5000);
   view.scrollTo(model.index(50000, 0), QAbstractItemView::PositionAtTop);
   QTRY_VERIFY(view.rowHeight(50000) > height);
   view.scrollToBottom();
   QTRY_VERIFY(view.rowHeight(model.rowCount() - 1) > height);
   QTRY_COMPARE(view.verticalScrollBar()->value(), view.verticalScrollBar()->maximum());
   model.replaceMessage("short");
   QTRY_COMPARE(view.rowHeight(50000), height);
   model.replaceMessage("<pre>a\nb\nc\nd</pre>");
   view.scrollToTop();
   QTRY_VERIFY(view.rowHeight(0) > height);
   view.setShowMultipleLines(false);
   QCOMPARE(view.rowHeight(0), height);
   QCOMPARE(view.rowHeight(50000), height);
   QFont font = view.font();
   font.setPointSize(font.pointSize() + 8);
   view.setFont(font);
   QVERIFY(view.rowHeight(0) > height);
}

void TestsLogViewer::documentCacheTracksContentAndFont()
{
   LargeModel model;
   TableLogItemDelegate delegate;
   QStyleOptionViewItem option;
   option.font = QApplication::font();
   const auto index = model.index(0, TableLogModel::MESSAGE);
   const QSize tall = delegate.sizeHint(option, index);
   model.message = "short"; // Even a reused index must not return another entry's size.
   const QSize shortSize = delegate.sizeHint(option, index);
   QVERIFY(shortSize.height() < tall.height());
   option.font.setPointSize(option.font.pointSize() + 12);
   QVERIFY(delegate.sizeHint(option, index).height() > shortSize.height());
   for (int row = 0; row < 6000; ++row)
      delegate.sizeHint(option, model.index(row, TableLogModel::MESSAGE));
   QCOMPARE(delegate.sizeHint(option, index), delegate.sizeHint(option, model.index(5999, TableLogModel::MESSAGE)));
}

void TestsLogViewer::refreshButtonKeepsExistingRows()
{
   QTemporaryDir dir;
   QVERIFY(dir.isValid());
   const QString originalDir = QDir::currentPath();
   const auto restoreDirectory = qScopeGuard([&] {
      QDir::setCurrent(originalDir);
      Common::Global::setDataFolderToDefault(Common::Global::DataFolderType::LOCAL);
   });
   QVERIFY(QDir::setCurrent(dir.path()));
   // The logger keeps its file open until process exit. Put it alongside test
   // build artifacts so it cannot prevent removal of the temporary input files.
   const QString logPath = QDir(QCoreApplication::applicationDirPath()).filePath("logviewer-test-data");
   QVERIFY(QDir().mkpath(logPath));
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, logPath);
   QFile writer(dir.filePath("001.log"));
   QVERIFY(writer.open(QIODevice::WriteOnly));
   for (int i = 0; i < 100; ++i)
      writer.write(line("needle", i % 2 ? "B" : "A"));
   writer.flush();
   QFile newer(dir.filePath("002.log"));
   QVERIFY(newer.open(QIODevice::WriteOnly));
   newer.write(line("newer file"));
   newer.close();

   MainWindow window;
   auto* pause = window.findChild<QPushButton*>("butPause");
   auto* refresh = window.findChild<QPushButton*>("butRefresh");
   auto* files = window.findChild<QComboBox*>("cmbFile");
   auto* view = window.findChild<QTableView*>("tblLog");
   QVERIFY(pause && refresh && files && view);
   pause->setChecked(true);
   QVERIFY(QMetaObject::invokeMethod(&window, "setCurrentFile", Q_ARG(QString, "001.log")));
   auto* model = qobject_cast<TableLogModel*>(view->model());
   QVERIFY(model);
   QTRY_COMPARE(model->rowCount(), 100);
   model->setFilter(model->getSeverities(), {"A"}, model->getThreads());
   model->search("needle");
   QCOMPARE(model->rowCount(), 50);
   view->setCurrentIndex(model->index(0, TableLogModel::MESSAGE));
   const QPersistentModelIndex selected(view->currentIndex());
   QSignalSpy resets(model, &QAbstractItemModel::modelReset);
   QSignalSpy inserted(model, &QAbstractItemModel::rowsInserted);
   QSignalSpy finished(model, &TableLogModel::loadingFinished);

   refresh->click(); // No new data: neither reload nor switch to 002.log.
   QTRY_VERIFY(!finished.isEmpty());
   QCOMPARE(files->currentText(), QString("001.log"));
   QCOMPARE(model->rowCount(), 50);
   QCOMPARE(resets.size(), 0);
   QCOMPARE(inserted.size(), 0);
   QCOMPARE(view->currentIndex(), QModelIndex(selected));

   writer.write(line("needle appended", "A") + line("filtered out", "B"));
   writer.flush();
   refresh->click();
   refresh->click(); // Coalesce repeated clicks; do not duplicate entries.
   QTRY_COMPARE(model->rowCount(), 51);
   QCOMPARE(model->currentNbFoundItems(), 51);
   QCOMPARE(resets.size(), 0);
   QCOMPARE(inserted.size(), 1);
   QVERIFY(selected.isValid());
   QCOMPARE(view->currentIndex(), QModelIndex(selected));
   QVERIFY(pause->isChecked());

   // A smaller, rewritten file genuinely requires a fresh model.
   QVERIFY(writer.resize(0));
   QVERIFY(writer.seek(0));
   writer.write(line("replacement"));
   writer.flush();
   refresh->click();
   QTRY_COMPARE(model->rowCount(), 1);
   QVERIFY(!resets.isEmpty());
   QCOMPARE(model->data(model->index(0, TableLogModel::MESSAGE)).toString(), QString("replacement"));
}

QTEST_MAIN(TestsLogViewer)
#include "TestsLogViewer.moc"
