#include <QTest>

#include <cstring>
#include <limits>

#include <google/protobuf/any.pb.h>

#include <Common/Constants.h>
#include <Common/Network/Message.h>
#include <Common/ZeroCopyStreamQIODevice.h>

namespace
{
   class WriteDevice : public QIODevice
   {
   public:
      WriteDevice() { this->open(QIODevice::WriteOnly | QIODevice::Unbuffered); }
      bool isSequential() const override { return true; }

      QByteArray data;
      qint64 maxWrite = std::numeric_limits<int>::max();
      qint64 failAfter = -1;
      qint64 failureResult = -1;
      bool recoverAfterFailure = false;
      int writeCalls = 0;

   protected:
      qint64 readData(char*, qint64) override { return -1; }
      qint64 writeData(const char* buffer, qint64 size) override
      {
         ++this->writeCalls;
         if (this->failAfter >= 0 && this->data.size() >= this->failAfter)
         {
            if (this->recoverAfterFailure)
               this->failAfter = -1;
            return this->failureResult;
         }

         qint64 written = qMin(size, this->maxWrite);
         if (this->failAfter >= 0)
            written = qMin(written, this->failAfter - this->data.size());
         this->data.append(buffer, written);
         return written;
      }
   };
}

class ZeroCopyOutputStreamQIODeviceTests : public QObject
{
   Q_OBJECT

private slots:
   void messageHeaderWriteFailure_data()
   {
      QTest::addColumn<int>("failAfter");
      QTest::addColumn<int>("failureResult");
      QTest::addColumn<bool>("withBody");
      for (int after : {0, 1, 3, 8})
         for (int result : {-1, 0})
            for (bool body : {false, true})
               QTest::newRow(qPrintable(QString("after=%1-result=%2-body=%3").arg(after).arg(result).arg(body)))
                  << after << result << body;
   }

   void messageHeaderWriteFailure()
   {
      QFETCH(int, failAfter);
      QFETCH(int, failureResult);
      QFETCH(bool, withBody);
      google::protobuf::Any message;
      message.set_value("must not be written after a failed header");
      const Common::MessageHeader header(Common::MessageHeader::GUI_REFRESH,
         withBody ? message.ByteSizeLong() : 0, Common::Hash());
      WriteDevice device;
      device.failAfter = failAfter;
      device.failureResult = failureResult;
      device.recoverAfterFailure = true;
      QCOMPARE(Common::Message::writeMessageToDevice(&device, header, withBody ? &message : nullptr), 0);
      QCOMPARE(device.data.size(), failAfter);
   }

   void byteCountAndBufferReuse()
   {
      WriteDevice device;
      Common::ZeroCopyOutputStreamQIODevice stream(&device);
      QCOMPARE(stream.ByteCount(), 0);
      QVERIFY(stream.Flush());
      QCOMPARE(device.writeCalls, 0);

      void* data;
      int size;
      QVERIFY(stream.Next(&data, &size));
      const int blockSize = size;
      QCOMPARE(stream.ByteCount(), blockSize);
      std::memcpy(data, "hello", 5);
      stream.BackUp(size - 5);
      QCOMPARE(stream.ByteCount(), 5);

      QVERIFY(stream.Next(&data, &size));
      QCOMPARE(size, blockSize - 5);
      QCOMPARE(device.writeCalls, 0);
      std::memcpy(data, " world", 6);
      stream.BackUp(size - 6);
      QCOMPARE(stream.ByteCount(), 11);
      QVERIFY(stream.Flush());
      QCOMPARE(device.data, QByteArray("hello world"));
      QCOMPARE(stream.ByteCount(), 11);
      const int writes = device.writeCalls;
      QVERIFY(stream.Flush());
      QCOMPARE(device.writeCalls, writes);
   }

   void serialization_data()
   {
      QTest::addColumn<int>("payloadSize");
      QTest::addColumn<int>("maxWrite");
      QTest::newRow("empty") << 0 << std::numeric_limits<int>::max();
      QTest::newRow("small") << 14 << std::numeric_limits<int>::max();
      QTest::newRow("multiple buffers") << 3 * Common::Constants::PROTOBUF_STREAMING_BUFFER_SIZE + 17
                                        << std::numeric_limits<int>::max();
      QTest::newRow("short writes") << 3 * Common::Constants::PROTOBUF_STREAMING_BUFFER_SIZE + 17 << 7;
   }

   void serialization()
   {
      QFETCH(int, payloadSize);
      QFETCH(int, maxWrite);
      google::protobuf::Any message;
      message.set_value(std::string(payloadSize, 'x'));
      const std::string expected = message.SerializeAsString();
      WriteDevice device;
      device.maxWrite = maxWrite;
      {
         Common::ZeroCopyOutputStreamQIODevice stream(&device);
         QVERIFY(message.SerializeToZeroCopyStream(&stream));
         QCOMPARE(stream.ByteCount(), expected.size());
         QVERIFY(message.SerializeToZeroCopyStream(&stream));
         QCOMPARE(stream.ByteCount(), 2 * expected.size());
         QVERIFY(stream.Flush());
         QCOMPARE(device.data, QByteArray::fromStdString(expected + expected));
      }
      QCOMPARE(device.data, QByteArray::fromStdString(expected + expected));
   }

   void destructorFlushes()
   {
      WriteDevice device;
      device.maxWrite = 2;
      {
         Common::ZeroCopyOutputStreamQIODevice stream(&device);
         void* data;
         int size;
         QVERIFY(stream.Next(&data, &size));
         std::memcpy(data, "hello", 5);
         stream.BackUp(size - 5);
      }
      QCOMPARE(device.data, QByteArray("hello"));
   }

   void writeFailure_data()
   {
      QTest::addColumn<int>("failureResult");
      QTest::addColumn<int>("failAfter");
      QTest::addColumn<int>("flushMethod");
      for (int method = 0; method < 3; ++method)
         for (int after : {0, 3})
            for (int result : {-1, 0})
               QTest::newRow(qPrintable(QString("method=%1 after=%2 result=%3").arg(method).arg(after).arg(result)))
                  << result << after << method;
   }

   void writeFailure()
   {
      QFETCH(int, failureResult);
      QFETCH(int, failAfter);
      QFETCH(int, flushMethod);
      WriteDevice device;
      device.failureResult = failureResult;
      device.failAfter = failAfter;
      int callsAfterFailure;
      {
         Common::ZeroCopyOutputStreamQIODevice stream(&device);
         void* data;
         int size;
         QVERIFY(stream.Next(&data, &size));
         std::memset(data, 'x', size);
         if (flushMethod == 0)
         {
            stream.BackUp(size - 10);
            QVERIFY(!stream.Flush());
         }
         else if (flushMethod == 1)
            QVERIFY(!stream.Next(&data, &size));
         else
            stream.BackUp(0);

         QCOMPARE(device.data, QByteArray(failAfter, 'x'));
         callsAfterFailure = device.writeCalls;
         QVERIFY(!stream.Flush());
         QVERIFY(!stream.Next(&data, &size));
         QVERIFY(!stream.Next(&data, &size));
         QCOMPARE(device.writeCalls, callsAfterFailure);
      }
      QCOMPARE(device.writeCalls, callsAfterFailure);
   }

   void messageFinalWriteFailure_data()
   {
      QTest::addColumn<int>("failureResult");
      QTest::newRow("error") << -1;
      QTest::newRow("no progress") << 0;
   }

   void messageFinalWriteFailure()
   {
      QFETCH(int, failureResult);
      google::protobuf::Any message;
      message.set_value("payload");
      const Common::MessageHeader header(Common::MessageHeader::CORE_IM_ALIVE, message.ByteSizeLong(), Common::Hash());
      WriteDevice device;
      device.failureResult = failureResult;
      device.failAfter = Common::MessageHeader::HEADER_SIZE + 3;
      QCOMPARE(Common::Message::writeMessageToDevice(&device, header, &message), 0);
      QCOMPARE(device.data.size(), Common::MessageHeader::HEADER_SIZE + 3);

      WriteDevice successfulDevice;
      QCOMPARE(Common::Message::writeMessageToDevice(&successfulDevice, header, &message),
               Common::MessageHeader::HEADER_SIZE + message.ByteSizeLong());
      QCOMPARE(successfulDevice.data.mid(Common::MessageHeader::HEADER_SIZE),
               QByteArray::fromStdString(message.SerializeAsString()));
   }
};

QTEST_GUILESS_MAIN(ZeroCopyOutputStreamQIODeviceTests)
#include "ZeroCopyOutputStreamQIODeviceTests.moc"
