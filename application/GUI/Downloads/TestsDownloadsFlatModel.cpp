#include <QtTest>
#include <QTemporaryDir>
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
      QList<GUI::DownloadFilterStatus> getFilteredValues() const override { return {}; }
   };

   class Model : public GUI::DownloadsFlatModel
   {
   public:
      using GUI::DownloadsFlatModel::DownloadsFlatModel;
      using GUI::DownloadsFlatModel::onNewState;
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
         this->model.onNewState(this->state);
         return this->model.getEta();
      }
   };
}

class TestsDownloadsFlatModel : public QObject
{
   Q_OBJECT

private slots:
   void startupAndRollingAverage()
   {
      Fixture f;
      for (int i = 0; i < 12; ++i)
         QCOMPARE(f.update(0), ETA_UNKNOWN);
      for (int i = 0; i < 9; ++i)
         QCOMPARE(f.update(1000), ETA_UNKNOWN);
      QCOMPARE(f.update(1000), quint64(10));
      // Replacing a positive sample must keep the nonzero count at ten.
      QCOMPARE(f.update(2000), quint64(9));
      for (int i = 0; i < 12; ++i)
         QCOMPARE(f.update(0), ETA_UNKNOWN);
   }

   void zeroMustLeaveTheWindowBeforeEtaReturns()
   {
      Fixture f;
      for (int i = 0; i < 10; ++i)
         f.update(1000);
      QCOMPARE(f.update(0), ETA_UNKNOWN);
      for (int i = 0; i < 9; ++i)
         QCOMPARE(f.update(1000), ETA_UNKNOWN);
      QCOMPARE(f.update(1000), quint64(10));
   }

   void slowRecoveryDoesNotDivideByZero()
   {
      Fixture f;
      for (int i = 0; i < 10; ++i)
         f.update(1000);
      for (int i = 0; i < 9; ++i)
         QCOMPARE(f.update(0), ETA_UNKNOWN);
      // Nine bytes spread across ten samples round down to a zero average.
      for (int i = 0; i < 9; ++i)
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
