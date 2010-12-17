#ifndef GUI_CORECONTROLLER_H
#define GUI_CORECONTROLLER_H

#include <QProcess>

namespace GUI
{
   class CoreController
   {
   public:
      static void StartCore();
      static void StopCore();

   private:
      static QProcess coreProcess; ///< Only used when unable to use service system.
   };
}

#endif
