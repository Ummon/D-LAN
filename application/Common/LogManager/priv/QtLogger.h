#ifndef QTLOGGER_H
#define QTLOGGER_H

#include <priv/Logger.h>

namespace LM
{
   class QtLogger : public Logger
   {
   public:
      static const QtLogger me;
      static void init();

   private:
      QtLogger();
   };
}

#endif
