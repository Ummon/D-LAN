#ifndef STDOUTLOGGER_H
#define STDOUTLOGGER_H

#include <QObject>
#include <QFile>

#include <priv/Logger.h>

namespace LM
{
   class StdLogger : public QObject, public Logger
   {
      Q_OBJECT
   private:
      static const int BUFFER_SIZE = 512;

   public:
      static const StdLogger stdoutLogger;
      static const StdLogger stderrLogger;
      static void init();

   private:
       StdLogger(FILE* file, const QString& name);

   private slots:
       void newData();

   private:
      int input[2];
      QFile stdoutIn;
      char buffer[BUFFER_SIZE];
   };
}

#endif
