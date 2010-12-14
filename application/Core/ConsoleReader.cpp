#include <ConsoleReader.h>
using namespace CoreSpace;

#include <QCoreApplication>

#include <Log.h>
#include <io.h> // From MingW.
#include <windows.h>

const QString ConsoleReader::QUIT_COMMAND("quit");

ConsoleReader::ConsoleReader(QObject *parent) :
    QThread(parent), inputStream(stdin), stopping(false)
{
}

void ConsoleReader::stop()
{
   this->stopping = true;
   // TODO : Don't know how to unblock 'readLine' which use 'fgets(..)' internaly...

   // fclose(stdin) // Don't work, blocks.

   // QIODevice* in = this->inputStream.device();
   // close(((QFile*)in)->handle()); // Don't work.

   // this->wait();
}

void ConsoleReader::run()
{
   while(!this->stopping)
   {
      // If AybabtuCore is a child process of AybabtuGUI and the latter is killed stdin is closed
      // In this case AybabtuCore must be terminated.
      if (feof(stdin))
      {
         emit newLine(QUIT_COMMAND);
         this->stopping = true;
         return;
      }

      QString str = this->inputStream.device()->readLine().trimmed();
      L_DEBU(QString("Command line : %1").arg(str));

      if (str.size() > 0)
         emit newLine(str);

      if (str == QUIT_COMMAND) // Cheating, see the 'stop()' method above.
         this->stopping = true;
   }
}
