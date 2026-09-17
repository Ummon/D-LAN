#include <QTest>
#include <QBuffer>
#include <QTemporaryFile>

#include <cstring>
#include <limits>
#include <memory>

#include <google/protobuf/any.pb.h>
#include <Common/ZeroCopyStreamQIODevice.h>

namespace
{
   constexpr int BUFFER_SIZE = Common::Constants::PROTOBUF_STREAMING_BUFFER_SIZE;

   class SequentialDevice : public QIODevice
   {
   public:
      explicit SequentialDevice(QByteArray bytes) : bytes(std::move(bytes)) { this->open(ReadOnly); }
      bool isSequential() const override { return true; }

   protected:
      qint64 readData(char* data, qint64 size) override
      {
         const auto count = qMin(size, qint64(this->bytes.size() - this->offset));
         std::memcpy(data, this->bytes.constData() + this->offset, count);
         this->offset += count;
         return count;
      }
      qint64 writeData(const char*, qint64) override { return -1; }

   private:
      QByteArray bytes;
      qsizetype offset = 0;
   };

   class FailingSkipDevice : public SequentialDevice
   {
   public:
      using SequentialDevice::SequentialDevice;
   protected:
      qint64 skipData(qint64) override { return -1; }
   };
}

class ZeroCopyInputStreamQIODeviceTests : public QObject
{
   Q_OBJECT

private slots:
   void boundedMessages_data()
   {
      QTest::addColumn<QString>("deviceType");
      QTest::addColumn<int>("payloadSize");
      for (const auto* device : {"buffer", "file", "sequential"})
         for (int size : {0, 12, BUFFER_SIZE - 3, BUFFER_SIZE, BUFFER_SIZE + 13, 3 * BUFFER_SIZE + 17})
            QTest::newRow(qPrintable(QString("%1-%2").arg(device).arg(size))) << QString(device) << size;
   }

   void boundedMessages()
   {
      QFETCH(QString, deviceType);
      QFETCH(int, payloadSize);
      google::protobuf::Any first, second;
      if (payloadSize)
         first.set_value(std::string(payloadSize, 'x'));
      second.set_value("second frame");
      const auto firstBytes = QByteArray::fromStdString(first.SerializeAsString());
      const auto secondBytes = QByteArray::fromStdString(second.SerializeAsString());
      const QByteArray trailer("raw stream bytes");
      const QByteArray bytes = firstBytes + secondBytes + trailer;
      std::unique_ptr<QIODevice> device;
      if (deviceType == "buffer")
      {
         auto buffer = std::make_unique<QBuffer>();
         buffer->setData(bytes);
         QVERIFY(buffer->open(QIODevice::ReadOnly));
         device = std::move(buffer);
      }
      else if (deviceType == "file")
      {
         auto file = std::make_unique<QTemporaryFile>();
         QVERIFY(file->open());
         QCOMPARE(file->write(bytes), bytes.size());
         QVERIFY(file->seek(0));
         device = std::move(file);
      }
      else
         device = std::make_unique<SequentialDevice>(bytes);

      for (const auto& expected : {firstBytes, secondBytes})
      {
         Common::ZeroCopyInputStreamQIODevice stream(device.get());
         google::protobuf::Any decoded;
         QVERIFY(decoded.ParseFromBoundedZeroCopyStream(&stream, expected.size()));
         QCOMPARE(QByteArray::fromStdString(decoded.SerializeAsString()), expected);
         QCOMPARE(stream.ByteCount(), expected.size());
      }
      QCOMPARE(device->readAll(), trailer);
   }

   void backupAndSkip()
   {
      QByteArray bytes(3 * BUFFER_SIZE, Qt::Uninitialized);
      for (qsizetype i = 0; i < bytes.size(); ++i)
         bytes[i] = 'a' + i % 26;
      QBuffer device(&bytes);
      QVERIFY(device.open(QIODevice::ReadOnly));
      {
         Common::ZeroCopyInputStreamQIODevice stream(&device);
         const void* data;
         int count;
         QVERIFY(stream.Next(&data, &count));
         QCOMPARE(count, BUFFER_SIZE);
         stream.BackUp(count - 7);
         QCOMPARE(stream.ByteCount(), 7);
         QVERIFY(stream.Skip(5));
         QCOMPARE(stream.ByteCount(), 12);
         QVERIFY(stream.Next(&data, &count));
         QCOMPARE(QByteArray(static_cast<const char*>(data), count), bytes.mid(12, BUFFER_SIZE - 12));
         stream.BackUp(count);
         QVERIFY(stream.Skip(BUFFER_SIZE + 3));
         QCOMPARE(stream.ByteCount(), BUFFER_SIZE + 15);
         QVERIFY(stream.Next(&data, &count));
         QCOMPARE(QByteArray(static_cast<const char*>(data), count), bytes.mid(BUFFER_SIZE + 15, BUFFER_SIZE));
         stream.BackUp(count - 4);
         QCOMPARE(stream.ByteCount(), BUFFER_SIZE + 19);
      }
      QCOMPARE(device.readAll(), bytes.mid(BUFFER_SIZE + 19));
   }

   void skipPastEnd()
   {
      SequentialDevice device("short data");
      Common::ZeroCopyInputStreamQIODevice stream(&device);
      QVERIFY(!stream.Skip(-1));
      QVERIFY(stream.Skip(0));
      QCOMPARE(stream.ByteCount(), 0);
      QVERIFY(!stream.Skip(std::numeric_limits<int>::max()));
      QCOMPARE(stream.ByteCount(), 10);
      const void* data;
      int count;
      QVERIFY(!stream.Next(&data, &count));
      QCOMPARE(stream.ByteCount(), 10);
   }

   void skipFailure()
   {
      FailingSkipDevice device("untouched");
      {
         Common::ZeroCopyInputStreamQIODevice stream(&device);
         QVERIFY(!stream.Skip(3));
         QCOMPARE(stream.ByteCount(), 0);
      }
      QCOMPARE(device.readAll(), QByteArray("untouched"));
   }
};

QTEST_GUILESS_MAIN(ZeroCopyInputStreamQIODeviceTests)
#include "ZeroCopyInputStreamQIODeviceTests.moc"
