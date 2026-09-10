#include <QElapsedTimer>
#include <QScopeGuard>
#include <QFileInfo>
#include <QFile>
#include <QTemporaryDir>
#include <QTest>
#include <qtservice.h>
#include <qtunixserversocket.h>
#include <qtunixsocket.h>

#include <cerrno>
#include <chrono>
#include <fcntl.h>
#include <thread>
#include <csignal>
#include <pthread.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

namespace
{
   int connectionFd = -1;
   QByteArray connectionPath;
   int lastConnectionFd = -1;
   bool trackServerSocket = false;
   bool failListen = false;
   bool listenAttempted = false;
   int serverFd = -1;
}

extern "C" int __real_bind(int fd, const sockaddr* address, socklen_t length);
extern "C" int __wrap_bind(int fd, const sockaddr* address, socklen_t length)
{
   if (trackServerSocket)
      serverFd = fd;
   return __real_bind(fd, address, length);
}

extern "C" int __real_listen(int fd, int backlog);
extern "C" int __wrap_listen(int fd, int backlog)
{
   if (trackServerSocket && fd == serverFd)
   {
      listenAttempted = true;
      if (failListen)
      {
         errno = EOPNOTSUPP;
         return -1;
      }
   }
   return __real_listen(fd, backlog);
}

// Supply a private connected socket instead of contacting an installed service.
extern "C" int __real_connect(int fd, const sockaddr* address, socklen_t length);
extern "C" int __wrap_connect(int fd, const sockaddr* address, socklen_t length)
{
   if (!connectionPath.isEmpty())
   {
      sockaddr_un redirected{};
      redirected.sun_family = AF_UNIX;
      qstrncpy(redirected.sun_path, connectionPath.constData(), sizeof(redirected.sun_path));
      lastConnectionFd = fd;
      return __real_connect(fd, reinterpret_cast<const sockaddr*>(&redirected), sizeof(redirected));
   }
   if (connectionFd < 0)
      return __real_connect(fd, address, length);
   const int result = dup2(connectionFd, fd);
   close(connectionFd);
   connectionFd = -1;
   return result < 0 ? -1 : 0;
}

class QtServiceUnixTests : public QObject
{
   Q_OBJECT

private slots:
   void socketPathLength_data()
   {
      QTest::addColumn<int>("byteCount");
      QTest::addColumn<bool>("unicode");
      const int capacity = sizeof(sockaddr_un{}.sun_path);
      QTest::newRow("maximum-valid") << capacity - 1 << false;
      QTest::newRow("no-space-for-terminator") << capacity << false;
      QTest::newRow("overlong") << capacity + 20 << false;
      QTest::newRow("unicode-maximum-valid") << capacity - 1 << true;
      QTest::newRow("unicode-too-long") << capacity << true;
   }

   void socketPathLength()
   {
      QFETCH(int, byteCount);
      QFETCH(bool, unicode);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      QString path = directory.path() + '/';
      const int padding = byteCount - QFile::encodeName(path).size();
      QVERIFY(padding > 0);
      if (unicode)
         path += QString(padding / 2, QChar(0x00e9)) + QString(padding % 2, 'x');
      else
         path += QString(padding, 'x');
      QCOMPARE(QFile::encodeName(path).size(), byteCount);
      const bool valid = byteCount < static_cast<int>(sizeof(sockaddr_un{}.sun_path));
      if (!valid)
      {
         // Reject the full path before unlink(), even if a file already exists there.
         QFile sentinel(path);
         QVERIFY(sentinel.open(QIODevice::WriteOnly));
         QCOMPARE(sentinel.write("preserve"), 8);
      }

      QtUnixServerSocket server;
      const auto closeServer = qScopeGuard([&] { server.close(); });
      server.setPath(path);
      const int serverError = errno;
      QCOMPARE(server.isListening(), valid);
      QtUnixSocket client;
      const bool connected = client.connectTo(path);
      const int clientError = errno;
      QCOMPARE(connected, valid);
      if (!valid)
      {
         QCOMPARE(serverError, ENAMETOOLONG);
         QCOMPARE(clientError, ENAMETOOLONG);
         QFile sentinel(path);
         QVERIFY(sentinel.open(QIODevice::ReadOnly));
         QCOMPARE(sentinel.readAll(), QByteArray("preserve"));
      }
      else
      {
         QVERIFY(QFileInfo::exists(path));
         client.close();
         server.close();
         QVERIFY(!QFileInfo::exists(path));
      }
   }

   void invalidSocketPath_data()
   {
      QTest::addColumn<bool>("embeddedNull");
      QTest::newRow("empty") << false;
      QTest::newRow("embedded-null") << true;
   }

   void invalidSocketPath()
   {
      QFETCH(bool, embeddedNull);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString originalPath = directory.filePath("original");
      QFile sentinel(originalPath);
      QVERIFY(sentinel.open(QIODevice::WriteOnly));
      sentinel.close();
      const QString path = embeddedNull ? originalPath + QChar(0) + "suffix" : QString();
      QtUnixServerSocket server;
      const auto closeServer = qScopeGuard([&] { server.close(); });
      server.setPath(path);
      const int serverError = errno;
      QVERIFY(!server.isListening());
      QCOMPARE(serverError, EINVAL);
      QtUnixSocket client;
      QVERIFY(!client.connectTo(path));
      QCOMPARE(errno, EINVAL);
      QVERIFY(QFileInfo::exists(originalPath));
   }

   void connectionBacklog_data()
   {
      QTest::addColumn<QString>("operation");
      QTest::newRow("status-times-out") << QString("status");
      QTest::newRow("command-times-out") << QString("command");
      QTest::newRow("queue-recovers") << QString("recover");
      QTest::newRow("connection-and-reply-share-deadline") << QString("shared-deadline");
   }

   void connectionBacklog()
   {
      QFETCH(QString, operation);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QByteArray path = directory.filePath("service.sock").toUtf8();
      sockaddr_un address{};
      address.sun_family = AF_UNIX;
      QVERIFY(path.size() < static_cast<qsizetype>(sizeof(address.sun_path)));
      qstrncpy(address.sun_path, path.constData(), sizeof(address.sun_path));
      const int listener = socket(AF_UNIX, SOCK_STREAM | SOCK_NONBLOCK, 0);
      QVERIFY(listener >= 0);
      const auto closeListener = qScopeGuard([&] { close(listener); });
      QVERIFY(bind(listener, reinterpret_cast<const sockaddr*>(&address), sizeof(address)) == 0);
      QVERIFY(listen(listener, 0) == 0);
      const int queued = socket(AF_UNIX, SOCK_STREAM | SOCK_NONBLOCK, 0);
      QVERIFY(queued >= 0);
      const auto closeQueued = qScopeGuard([&] { close(queued); });
      // Linux permits one pending connection even with backlog zero.
      QVERIFY(__real_connect(queued, reinterpret_cast<const sockaddr*>(&address), sizeof(address)) == 0);

      connectionPath = path; // Exercise the public controller without contacting a real service.
      const auto resetPath = qScopeGuard([] { connectionPath.clear(); });
      std::thread peer;
      if (operation == "recover" || operation == "shared-deadline")
         peer = std::thread([&]
         {
            std::this_thread::sleep_for(std::chrono::milliseconds(operation == "recover" ? 100 : 2000));
            const int first = accept(listener, nullptr, nullptr);
            if (first >= 0)
               close(first);
            // Accept the retry, then keep the connection open without replying.
            pollfd incoming{listener, POLLIN, 0};
            if (poll(&incoming, 1, 4000) <= 0)
               return;
            const int client = accept(listener, nullptr, nullptr);
            if (client < 0)
               return;
            const auto closeClient = qScopeGuard([&] { close(client); });
            QDeadlineTimer deadline(4000);
            char buffer[128];
            while (!deadline.hasExpired())
            {
               pollfd fd{client, POLLIN, 0};
               if (poll(&fd, 1, static_cast<int>(deadline.remainingTime())) <= 0 ||
                   read(client, buffer, sizeof(buffer)) <= 0)
                  return;
            }
         });
      const auto joinPeer = qScopeGuard([&] { if (peer.joinable()) peer.join(); });

      QElapsedTimer timer;
      timer.start();
      bool result;
      if (operation == "recover")
      {
         QtUnixSocket socket;
         result = socket.connectTo(QString::fromUtf8(path));
      }
      else
      {
         QtServiceController controller("D-LAN unit test");
         result = operation == "status" ? controller.isRunning() : controller.stop();
      }
      const qint64 elapsed = timer.elapsed();
      if (peer.joinable())
         peer.join();
      QCOMPARE(result, operation == "recover");
      QVERIFY(elapsed < 4500);
      if (operation != "recover")
         QVERIFY(elapsed >= 2800);
      // Both timed-out and successful temporary clients release their descriptor.
      QVERIFY(lastConnectionFd >= 0);
      errno = 0;
      QCOMPARE(fcntl(lastConnectionFd, F_GETFD), -1);
      QCOMPARE(errno, EBADF);
   }

   void serverSetupFailure_data()
   {
      QTest::addColumn<bool>("listenFailure");
      QTest::newRow("bind-failure") << false;
      QTest::newRow("listen-failure") << true;
   }

   void serverSetupFailure()
   {
      QFETCH(bool, listenFailure);
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      const QString path = directory.filePath(listenFailure ? "service.sock" : "missing/service.sock");
      QtUnixServerSocket server;
      trackServerSocket = true;
      failListen = listenFailure;
      const auto cleanup = qScopeGuard([]
      {
         // Keep a failing regression test from leaking its own descriptor.
         if (serverFd >= 0 && fcntl(serverFd, F_GETFD) != -1)
            close(serverFd);
         serverFd = -1;
         trackServerSocket = false;
         failListen = false;
      });
      for (int attempt = 0; attempt < 8; ++attempt)
      {
         listenAttempted = false;
         server.setPath(path);
         QVERIFY(serverFd >= 0);
         QCOMPARE(listenAttempted, listenFailure);
         QVERIFY(!server.isListening());
         errno = 0;
         const int descriptorFlags = fcntl(serverFd, F_GETFD);
         const int descriptorError = errno;
         QCOMPARE(descriptorFlags, -1);
         QCOMPARE(descriptorError, EBADF);
         QVERIFY(!QFileInfo::exists(path));
      }
   }

   void commandReply_data()
   {
      QTest::addColumn<QString>("command");
      QTest::addColumn<QString>("behavior");
      QTest::addColumn<bool>("expected");
      QTest::newRow("stop") << QString("terminate") << QString("success") << true;
      QTest::newRow("pause") << QString("pause") << QString("success") << true;
      QTest::newRow("resume") << QString("resume") << QString("success") << true;
      QTest::newRow("custom-command") << QString("num:42") << QString("success") << true;
      QTest::newRow("fragmented-reply") << QString("terminate") << QString("fragmented") << true;
      QTest::newRow("negative-reply") << QString("terminate") << QString("negative") << false;
      QTest::newRow("invalid-reply") << QString("terminate") << QString("invalid") << false;
      QTest::newRow("disconnect") << QString("terminate") << QString("disconnect") << false;
      QTest::newRow("silent-service") << QString("terminate") << QString("silent") << false;
      QTest::newRow("incomplete-slow-reply") << QString("terminate") << QString("slow") << false;
   }

   void commandReply()
   {
      QFETCH(QString, command);
      QFETCH(QString, behavior);
      QFETCH(bool, expected);
      int pair[2];
      QVERIFY(socketpair(AF_UNIX, SOCK_STREAM, 0, pair) == 0);
      connectionFd = pair[0];
      const auto closeUnusedConnection = qScopeGuard([]
      {
         if (connectionFd >= 0)
            close(connectionFd);
         connectionFd = -1;
      });
      QByteArray receivedCommand;
      std::thread peer([&]
      {
         // The timeout cases deliberately let the client close first.
         sigset_t blockedSignals;
         sigemptyset(&blockedSignals);
         sigaddset(&blockedSignals, SIGPIPE);
         pthread_sigmask(SIG_BLOCK, &blockedSignals, nullptr);
         const auto closePeer = qScopeGuard([&] { close(pair[1]); });
         char buffer[128];
         while (!receivedCommand.endsWith("\r\n"))
         {
            pollfd fd{pair[1], POLLIN, 0};
            if (poll(&fd, 1, 5000) <= 0)
               return;
            const ssize_t count = read(pair[1], buffer, sizeof(buffer));
            if (count <= 0)
               return;
            receivedCommand.append(buffer, count);
         }
         if (behavior == "disconnect")
            return;
         if (behavior == "success")
            write(pair[1], "true", 4);
         else if (behavior == "negative")
            write(pair[1], "false", 5);
         else if (behavior == "invalid")
            write(pair[1], "wrong", 5);
         else if (behavior == "fragmented")
         {
            write(pair[1], "tr", 2);
            std::this_thread::sleep_for(std::chrono::milliseconds(30));
            write(pair[1], "ue", 2);
         }
         else if (behavior == "slow")
         {
            for (char byte : QByteArray("tru"))
            {
               std::this_thread::sleep_for(std::chrono::milliseconds(800));
               if (write(pair[1], &byte, 1) != 1)
                  return;
            }
         }
         // Stay connected without sending anything else until the client exits.
         pollfd fd{pair[1], POLLIN, 0};
         poll(&fd, 1, 6000);
      });
      auto joinPeer = qScopeGuard([&] { peer.join(); });
      QtServiceController controller("D-LAN unit test");
      QElapsedTimer timer;
      timer.start();
      const bool result = command == "terminate" ? controller.stop() :
         command == "pause" ? controller.pause() :
         command == "resume" ? controller.resume() : controller.sendCommand(42);
      const qint64 elapsed = timer.elapsed();
      peer.join();
      joinPeer.dismiss();
      QCOMPARE(result, expected);
      if (behavior == "silent" || behavior == "slow")
      {
         QVERIFY(elapsed >= 2800);
         QVERIFY(elapsed < 4500); // One deadline, not a new timeout for every fragment.
      }
      QCOMPARE(receivedCommand, command.toLatin1() + "\r\n");
   }
};

QTEST_GUILESS_MAIN(QtServiceUnixTests)
#include "QtServiceUnixTests.moc"
