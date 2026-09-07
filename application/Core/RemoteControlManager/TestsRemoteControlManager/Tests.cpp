#include <limits>

#include <QTest>

#include <Core/PeerManager/GetChunkParams.h>
#include <priv/UploadProgress.h>

class Tests : public QObject
{
   Q_OBJECT

private slots:
   void uploadProgress_data()
   {
      QTest::addColumn<quint64>("size");
      QTest::addColumn<quint64>("owned");
      QTest::addColumn<int>("offset");
      QTest::addColumn<int>("expected");

      const quint64 maximum = std::numeric_limits<quint64>::max();
      const quint64 signedMaximum = std::numeric_limits<qint64>::max();
      QTest::newRow("empty") << quint64(0) << maximum << 1 << 0;
      QTest::newRow("not-started") << quint64(100) << quint64(0) << 0 << 0;
      QTest::newRow("partial") << quint64(100) << quint64(20) << 5 << 2500;
      QTest::newRow("fraction") << quint64(3) << quint64(1) << 0 << 3333;
      QTest::newRow("complete") << quint64(100) << quint64(90) << 10 << 10000;
      QTest::newRow("offset-exceeds-remaining") << quint64(100) << quint64(90) << 20 << 10000;
      QTest::newRow("negative-offset") << quint64(100) << quint64(25) << -1 << 2500;
      QTest::newRow("untrusted-maximum") << quint64(100) << maximum << 1 << 10000;
      QTest::newRow("signed-addition-overflow") << signedMaximum << signedMaximum << 1 << 10000;
      QTest::newRow("multiplication-overflow") << (quint64(1) << 62) << (quint64(1) << 61) << 0 << 5000;
      QTest::newRow("unsigned-addition-overflow") << maximum << (maximum - 1) << 2 << 10000;
      QTest::newRow("maximum-incomplete") << maximum << (maximum - 1) << 0 << 9999;
   }

   void uploadProgress()
   {
      QFETCH(quint64, size);
      QFETCH(quint64, owned);
      QFETCH(int, offset);
      QFETCH(int, expected);

      const PM::GetChunkParams params({}, offset, 0, owned);
      QCOMPARE(params.getFileBytesOwnedByPeer(), owned);
      QCOMPARE(RCM::uploadProgress(size, params.getFileBytesOwnedByPeer(), params.getOffset()), expected);
   }
};

QTEST_APPLESS_MAIN(Tests)
#include "Tests.moc"
