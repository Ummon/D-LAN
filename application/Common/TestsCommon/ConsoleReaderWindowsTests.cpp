#include <QScopeGuard>
#include <QSignalSpy>
#include <QTest>

#include <Common/ConsoleReader.h>

#include <cstdio>
#include <fcntl.h>
#include <io.h>

class ConsoleReaderWindowsTests : public QObject
{
   Q_OBJECT

private slots:
   void eofStopsReading_data()
   {
      QTest::addColumn<QByteArray>("input");
      QTest::addColumn<QStringList>("expectedLines");
      QTest::addColumn<int>("nbLinesRead");
      QTest::newRow("empty-input") << QByteArray() << QStringList() << 0;
      QTest::newRow("blank-lines") << QByteArray("\n   \n") << QStringList() << 2;
      QTest::newRow("complete-lines") << QByteArray(" help \r\nquit\n") << QStringList{"help", "quit"} << 2;
      QTest::newRow("blank-line-before-command") << QByteArray("\nhelp\n") << QStringList{"help"} << 2;
      QTest::newRow("last-line-without-newline") << QByteArray("help\n quit ") << QStringList{"help", "quit"} << 2;
   }

   void eofStopsReading()
   {
      QFETCH(QByteArray, input);
      QFETCH(QStringList, expectedLines);
      QFETCH(int, nbLinesRead);

      const int stdinFd = _fileno(stdin);
      const int savedInput = _dup(stdinFd);
      QVERIFY(savedInput >= 0);
      const auto restoreInput = qScopeGuard([&]
      {
         _dup2(savedInput, stdinFd);
         _close(savedInput);
         clearerr(stdin);
      });

      int pipeFds[2];
      QVERIFY(_pipe(pipeFds, 4096, _O_BINARY) == 0);
      QCOMPARE(_write(pipeFds[1], input.constData(), static_cast<unsigned int>(input.size())), static_cast<int>(input.size()));
      _close(pipeFds[1]);
      QCOMPARE(_dup2(pipeFds[0], stdinFd), 0);
      _close(pipeFds[0]);
      clearerr(stdin);

      Common::ConsoleReader reader;
      QSignalSpy lines(&reader, &Common::ConsoleReader::newLine);
      // One request per line read, the initial one is emitted by the constructor.
      QSignalSpy readRequests(&reader, &Common::ConsoleReader::readNextLine);

      QTRY_COMPARE_WITH_TIMEOUT(readRequests.size(), nbLinesRead, 1000);
      QStringList actualLines;
      for (const auto& arguments : lines)
         actualLines << arguments.first().toString();
      QCOMPARE(actualLines, expectedLines);

      // At EOF the reader must stop instead of reading again and again.
      QTest::qWait(50);
      QCOMPARE(readRequests.size(), nbLinesRead);
   }
};

QTEST_GUILESS_MAIN(ConsoleReaderWindowsTests)
#include "ConsoleReaderWindowsTests.moc"
