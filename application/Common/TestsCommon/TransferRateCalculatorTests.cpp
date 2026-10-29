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

   /**
     * One sample per second, the newest first. Seconds without any update are 0,
     * and the sample saved just before such a gap is kept.
     */
   void history()
   {
      using Rates = QList<quint64>;
      Common::TransferRateCalculator calculator;
      calculator.addData(30000);

      QTest::qSleep(150); // Second 0.
      QCOMPARE(calculator.getTransferRateAt(0), quint64(10000));
      QCOMPARE(calculator.getTransferRatesRange(0, 3), Rates({ 10000, 0, 0 }));

      QTest::qSleep(2500); // Second 2, second 1 had no update.
      QCOMPARE(calculator.getTransferRatesRange(0, 3), Rates({ 10000, 0, 10000 }));

      QTest::qSleep(2000); // Second 4, the data has left the averaging window.
      QCOMPARE(calculator.getTransferRatesRange(0, 5), Rates({ 0, 0, 10000, 0, 10000 }));

      QTest::qSleep(3500); // Second 8, idle for longer than the averaging period.
      QCOMPARE(calculator.getTransferRateAt(6), quint64(10000));
      QCOMPARE(calculator.getTransferRatesRange(0, 9), Rates({ 0, 0, 0, 0, 0, 0, 10000, 0, 10000 }));
      QCOMPARE(calculator.getTransferRatesRange(7, 3), Rates({ 0, 10000, 0 }));
      QCOMPARE(calculator.getTransferRatesRange(0, 0), Rates());
      QCOMPARE(calculator.getTransferRateAt(20 * 60), quint64(0));
   }
};

QTEST_GUILESS_MAIN(TransferRateCalculatorTests)
#include "TransferRateCalculatorTests.moc"
