#include <QCoreApplication>
#include <QElapsedTimer>
#include <QScopeGuard>
#include <QSemaphore>
#include <QTest>

#include <functional>
#include <memory>
#include <algorithm>

#include <Common/Settings.h>
#include <Core/UploadManager/priv/ChunksUploader.h>
#include <Protos/core_settings.pb.h>

namespace
{
   class Reader : public FM::IDataReader
   {
   public:
      int calls = 0;
      std::function<void()> onRead;

      int read(char* buffer, uint offset) override
      {
         ++this->calls;
         if (this->onRead)
            this->onRead();
         if (offset >= 32)
            return 0;
         std::fill_n(buffer, 32 - offset, 'x');
         return 32 - offset;
      }
   };

   class Chunk : public FM::IChunk
   {
   public:
      QSharedPointer<Reader> reader = QSharedPointer<Reader>::create();
      int opens = 0;

      QSharedPointer<FM::IDataReader> getDataReader() override { ++this->opens; return this->reader; }
      QSharedPointer<FM::IDataWriter> getDataWriter() override { return {}; }
      void removeItsIncompleteFile() override {}
      bool populateEntry(Protos::Common::Entry*) const override { return false; }
      Common::Path getFilePath() const override { return {}; }
      int getNum() const override { return 0; }
      int getNbTotalChunk() const override { return 1; }
      Common::Hash getHash() const override { return {}; }
      void setHash(const Common::Hash&) override {}
      int getKnownBytes() const override { return 32; }
      void setKnownBytes(int) override {}
      int getChunkSize() const override { return 32; }
      bool isComplete() const override { return true; }
      QString toStringLog() const override { return "test chunk"; }
   };

   class Socket : public PM::ISocket
   {
   public:
      qint64 pending = 0;
      int writes = 0;
      QList<int> waits;
      QSemaphore enteredWait;
      bool closed = false;
      bool immediateFailure = false;
      bool progress = false;
      QThread* owner = nullptr;

      void setReadBufferSize(qint64) override {}
      qint64 bytesAvailable() const override { return 0; }
      qint64 read(char*, qint64) override { return 0; }
      QByteArray readAll() override { return {}; }
      bool waitForReadyRead(int) override { return false; }
      qint64 bytesToWrite() const override { return this->pending; }
      qint64 write(const char*, qint64 size) override
      {
         ++this->writes;
         this->pending = 4096;
         return size;
      }
      qint64 write(const QByteArray& bytes) override { return this->write(bytes.constData(), bytes.size()); }
      bool waitForBytesWritten(int msecs) override
      {
         this->waits << msecs;
         this->enteredWait.release();
         if (this->immediateFailure)
            return false;
         QThread::msleep(msecs);
         // Two bursts of progress separated by three wait intervals each. The total duration
         // exceeds socket_timeout, but neither period without progress does.
         if (this->progress && this->waits.size() % 3 == 0)
         {
            this->pending -= 2048;
            return true;
         }
         return false;
      }
      void moveToThread(QThread* thread) override { this->owner = thread; }
      QString errorString() const override { return "test socket timeout"; }
      Common::Hash getRemotePeerID() const override { return {}; }
      void finished(bool close) override { this->closed = close; }
   };
}

class Tests : public QObject
{
   Q_OBJECT

private slots:
   void init()
   {
      SETTINGS.set("socket_timeout", quint32(350));
   }

   void stopBeforeRun()
   {
      auto chunk = QSharedPointer<Chunk>::create();
      auto socket = QSharedPointer<Socket>::create();
      Common::TransferRateCalculator rate;
      UM::ChunksUploader upload({PM::GetChunkParams(chunk, 0, 32, 0)}, socket, rate);
      upload.stop();
      upload.run();
      upload.finished();
      QCOMPARE(chunk->opens, 0);
      QCOMPARE(socket->writes, 0);
      QVERIFY(socket->closed);
      QCOMPARE(socket->owner, QThread::currentThread());
   }

   void stopDuringRead()
   {
      auto chunk = QSharedPointer<Chunk>::create();
      auto socket = QSharedPointer<Socket>::create();
      Common::TransferRateCalculator rate;
      UM::ChunksUploader upload({PM::GetChunkParams(chunk, 0, 32, 0)}, socket, rate);
      chunk->reader->onRead = [&] { upload.stop(); };
      upload.run();
      upload.finished();
      QCOMPARE(chunk->reader->calls, 1);
      QCOMPARE(socket->writes, 0);
      QVERIFY(socket->closed);
   }

   void stopDuringSocketWait()
   {
      SETTINGS.set("socket_timeout", quint32(7000));
      auto chunk = QSharedPointer<Chunk>::create();
      auto socket = QSharedPointer<Socket>::create();
      Common::TransferRateCalculator rate;
      UM::ChunksUploader upload({PM::GetChunkParams(chunk, 0, 32, 0)}, socket, rate);
      std::unique_ptr<QThread> worker(QThread::create([&] { upload.run(); }));
      const auto join = qScopeGuard([&] { upload.stop(); worker->wait(); });
      upload.init(worker.get());
      worker->start();
      QVERIFY(socket->enteredWait.tryAcquire(1, 2000));
      upload.stop();
      QVERIFY2(worker->wait(1000), "Cancellation waited for the full socket timeout");
      upload.finished();
      QVERIFY(socket->closed);
      QCOMPARE(socket->owner, QThread::currentThread());
      QVERIFY(!socket->waits.isEmpty());
      for (int wait : socket->waits)
         QVERIFY(wait > 0 && wait <= 100);
   }

   void stalledSocket_data()
   {
      QTest::addColumn<bool>("immediateFailure");
      QTest::newRow("blocking timeout") << false;
      QTest::newRow("immediate error") << true;
   }

   void stalledSocket()
   {
      QFETCH(bool, immediateFailure);
      auto chunk = QSharedPointer<Chunk>::create();
      auto socket = QSharedPointer<Socket>::create();
      socket->immediateFailure = immediateFailure;
      Common::TransferRateCalculator rate;
      UM::ChunksUploader upload({PM::GetChunkParams(chunk, 0, 32, 0)}, socket, rate);
      QElapsedTimer elapsed;
      elapsed.start();
      upload.run();
      QVERIFY(elapsed.elapsed() >= 350); // A single 100 ms timeout must not terminate the upload.
      QVERIFY(elapsed.elapsed() < 2000);
      upload.finished();
      QVERIFY(socket->closed);
      QVERIFY(socket->waits.size() >= 2);
      QVERIFY(socket->waits.size() < 10); // Immediate errors must not spin.
      for (int wait : socket->waits)
         QVERIFY(wait > 0 && wait <= 100);
   }

   void progressRestartsTimeout()
   {
      SETTINGS.set("socket_timeout", quint32(500));
      auto chunk = QSharedPointer<Chunk>::create();
      auto socket = QSharedPointer<Socket>::create();
      socket->progress = true;
      Common::TransferRateCalculator rate;
      UM::ChunksUploader upload({PM::GetChunkParams(chunk, 0, 32, 0)}, socket, rate);
      upload.run();
      upload.finished();
      QVERIFY(!socket->closed);
      QCOMPARE(socket->waits.size(), 6);
      QCOMPARE(upload.getChunks().first().getOffset(), 32);
   }
};

int main(int argc, char** argv)
{
   QCoreApplication app(argc, argv);
   auto* settings = new Protos::Core::Settings;
   settings->set_buffer_size_reading(32);
   settings->set_socket_buffer_size(16);
   settings->set_upload_lifetime(5000);
   SETTINGS.setSettingsMessage(settings);
   int result;
   {
      Tests tests;
      result = QTest::qExec(&tests, argc, argv);
   }
   SETTINGS.free();
   return result;
}

#include "Tests.moc"
