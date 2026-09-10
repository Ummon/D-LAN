#include <QElapsedTimer>
#include <QScopeGuard>
#include <QFileInfo>
#include <QTemporaryDir>
#include <QTest>
#include <qtservice.h>
#include <qtunixserversocket.h>

#include <cerrno>
#include <chrono>
#include <fcntl.h>
#include <thread>
#include <csignal>
#include <pthread.h>
#include <poll.h>
#include <sys/socket.h>
#include <unistd.h>

namespace
{
   int connectionFd = -1;
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
