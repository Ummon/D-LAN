#include <QCoreApplication>
#include <QEvent>
#include <QScopeGuard>
#include <QSignalSpy>
#include <QSocketNotifier>
#include <QTest>

#include <Common/ConsoleReader.h>

#include <cstdio>
#include <unistd.h>

class ConsoleReaderTests : public QObject
{
   Q_OBJECT

private:
   int activations = 0;

   bool eventFilter(QObject* object, QEvent* event) override
   {
      const auto notifier = qobject_cast<QSocketNotifier*>(object);
      if (notifier && notifier->socket() == STDIN_FILENO && event->type() == QEvent::SockAct)
         ++this->activations;
      return false;
   }

private slots:
   void eofStopsNotifications_data()
   {
      QTest::addColumn<QByteArray>("input");
      QTest::addColumn<QStringList>("expectedLines");
      QTest::newRow("empty-input") << QByteArray() << QStringList();
      QTest::newRow("blank-lines") << QByteArray("\n   \n") << QStringList();
      QTest::newRow("complete-lines") << QByteArray(" help \nquit\n") << QStringList{"help", "quit"};
      QTest::newRow("last-line-without-newline") << QByteArray("help\n quit ") << QStringList{"help", "quit"};
   }

   void eofStopsNotifications()
   {
      QFETCH(QByteArray, input);
      QFETCH(QStringList, expectedLines);
      const int savedInput = dup(STDIN_FILENO);
      QVERIFY(savedInput >= 0);
      const auto restoreInput = qScopeGuard([&]
      {
         dup2(savedInput, STDIN_FILENO);
         close(savedInput);
         clearerr(stdin);
      });
      int pipeFds[2];
      QVERIFY(pipe(pipeFds) == 0);
      const auto closePipe = qScopeGuard([&]
      {
         close(pipeFds[0]);
         if (pipeFds[1] >= 0)
            close(pipeFds[1]);
      });
      QCOMPARE(write(pipeFds[1], input.constData(), input.size()), static_cast<ssize_t>(input.size()));
      close(pipeFds[1]);
      pipeFds[1] = -1;
      QCOMPARE(dup2(pipeFds[0], STDIN_FILENO), STDIN_FILENO);
      clearerr(stdin);

      this->activations = 0;
      QCoreApplication::instance()->installEventFilter(this);
      const auto removeFilter = qScopeGuard([&] { QCoreApplication::instance()->removeEventFilter(this); });
      Common::ConsoleReader reader;
      QSignalSpy lines(&reader, &Common::ConsoleReader::newLine);
      QTest::qWait(30);
      QStringList actualLines;
      for (const auto& arguments : lines)
         actualLines << arguments.first().toString();
      QCOMPARE(actualLines, expectedLines);
      QVERIFY(this->activations > 0);

      // EOF stays readable at the OS level, but must stop generating callbacks.
      const int finishedActivations = this->activations;
      QTest::qWait(30);
      QCOMPARE(this->activations, finishedActivations);
   }
};

QTEST_GUILESS_MAIN(ConsoleReaderTests)
#include "ConsoleReaderTests.moc"
