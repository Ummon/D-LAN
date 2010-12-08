#ifndef CORE_CONSOLEREADER_H
#define CORE_CONSOLEREADER_H

#include <QThread>
#include <QTextStream>
#include <QIODevice>

namespace CoreSpace
{
   class ConsoleReader : public QThread
   {
      Q_OBJECT
   public:
      explicit ConsoleReader(QObject* parent = 0);
      void stop();

   protected:
      void run();

   signals:
      void newLine(QString line);

   private:
      QTextStream inputStream;
      bool stopping;
   };
}

#endif
