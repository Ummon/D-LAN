#include <ConsoleReader.h>
using namespace CoreSpace;

#include <QCoreApplication>

#include <Log.h>
#include <io.h> // From MingW.
#include <windows.h>


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
      QString str = this->inputStream.device()->readLine().trimmed();
      L_DEBU(QString("Commnand line : %1").arg(str));
      emit newLine(str);

      if (str == "quit") // Cheating, see the 'stop()' method above.
         this->stopping = true;
   }
}
