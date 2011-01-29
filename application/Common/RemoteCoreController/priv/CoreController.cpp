/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <priv/CoreController.h>
using namespace RCC;

#include <QtServiceController>
#include <QProcessEnvironment>

#include <Common/Constants.h>

#include <priv/Log.h>

CoreStatus CoreController::StartCore()
{
   QtServiceController controller(Common::SERVICE_NAME);
   if (!controller.isInstalled())
   {
      if (!QtServiceController::install("AybabtuCore.exe"))
         L_USER("Aybabtu Core cannot be installed as a service");
   }

   bool isRunning = false;

   if (!(isRunning = controller.isRunning()))
   {
      if (!(isRunning = controller.start()))
      {
         L_WARN("Aybabtu Core service cannot be launched. Trying to launch it as a subprocess..");
         if (coreProcess.state() == QProcess::NotRunning)
         {
            coreProcess.start("AybabtuCore.exe -e");
            return RUNNING_AS_SUB_PROCESS;
         }
      }
   }

   if (isRunning)
      return RUNNING_AS_SERVICE;
   return NOT_RUNNING;
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
