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
      QCOMPARE(unpolled.getTransferRate(), quint64(0));
      QCOMPARE(polled.getTransferRate(), quint64(0));

      QTest::qSleep(idleMs);
      // Reading the rate immediately before the same transfer must not change
      // how long its bytes remain in the three-second averaging window.
      QCOMPARE(polled.getTransferRate(), quint64(0));
      unpolled.addData(30029);
      polled.addData(30029);

      // Allow the current 100 ms bucket to close before checking the rate.
      QTest::qSleep(150);
      QCOMPARE(unpolled.getTransferRate(), quint64(10009));
      QCOMPARE(polled.getTransferRate(), quint64(10009));

      QTest::qSleep(2000);
      QCOMPARE(unpolled.getTransferRate(), quint64(10009));
      QCOMPARE(polled.getTransferRate(), quint64(10009));

      QTest::qSleep(1100);
      QCOMPARE(unpolled.getTransferRate(), quint64(0));
      QCOMPARE(polled.getTransferRate(), quint64(0));
   }

   /**
     * Neither the amount added at once nor the rate is limited to 32 bits.
     */
   void rateBeyond32Bits()
   {
      Common::TransferRateCalculator calculator;
      calculator.addData(Q_INT64_C(30000000000)); // 10 GB/s over the three-second averaging window.

      // Allow the current 100 ms bucket to close before checking the rate.
      QTest::qSleep(150);
      QCOMPARE(calculator.getTransferRate(), quint64(10000000000));
   }
};

QTEST_GUILESS_MAIN(TransferRateCalculatorTests)
#include "TransferRateCalculatorTests.moc"
