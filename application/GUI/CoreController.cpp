#include <CoreController.h>
using namespace GUI;

#include <QtServiceController>
#include <QProcessEnvironment>

#include <Common/Constants.h>

#include <Log.h>

void CoreController::StartCore()
{
   QtServiceController controller(Common::SERVICE_NAME);
   if (!controller.isInstalled())
   {
      if (!QtServiceController::install("AybabtuCore.exe"))
         L_USER("Aybabtu Core cannot be installed as a service");
   }

   if (!controller.isRunning())
   {
      if (!controller.start())
      {
         L_USER("Aybabtu Core service cannot be launched. Trying to launch it as a subprocess..");
         if (coreProcess.state() == QProcess::NotRunning)
         {
            coreProcess.start("AybabtuCore.exe -e");
         }
      }
   }
}

void CoreController::StopCore()
{
   QtServiceController controller(Common::SERVICE_NAME);
   if (controller.isRunning())
      controller.stop();

   if (coreProcess.state() == QProcess::Running)
   {
      coreProcess.write("quit\n");
      coreProcess.waitForFinished(2000);
   }
}

QProcess CoreController::coreProcess;
