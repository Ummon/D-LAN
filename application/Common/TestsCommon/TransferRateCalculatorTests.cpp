#include <QTest>

#include <Common/TransferRateCalculator.h>

class TransferRateCalculatorTests : public QObject
{
   Q_OBJECT

private slots:
   void burstAfterIdle_data()
   {
      QTest::addColumn<int>("idleMs");
      QTest::newRow("within averaging period") << 1200;
      QTest::newRow("beyond averaging period") << 3200;
   }

   void burstAfterIdle()
   {
      QFETCH(int, idleMs);
      Common::TransferRateCalculator unpolled;
      Common::TransferRateCalculator polled;
      QCOMPARE(unpolled.getTransferRate(), 0);
      QCOMPARE(polled.getTransferRate(), 0);

      QTest::qSleep(idleMs);
      // Reading the rate immediately before the same transfer must not change
      // how long its bytes remain in the three-second averaging window.
      QCOMPARE(polled.getTransferRate(), 0);
      unpolled.addData(30029);
      polled.addData(30029);

      // Allow the current 100 ms bucket to close before checking the rate.
      QTest::qSleep(150);
      QCOMPARE(unpolled.getTransferRate(), 10009);
      QCOMPARE(polled.getTransferRate(), 10009);

      QTest::qSleep(2000);
      QCOMPARE(unpolled.getTransferRate(), 10009);
      QCOMPARE(polled.getTransferRate(), 10009);

      QTest::qSleep(1100);
      QCOMPARE(unpolled.getTransferRate(), 0);
      QCOMPARE(polled.getTransferRate(), 0);
   }
};

QTEST_GUILESS_MAIN(TransferRateCalculatorTests)
#include "TransferRateCalculatorTests.moc"
