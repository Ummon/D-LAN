/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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

#ifdef Q_OS_WIN32
   const QString CoreController::CORE_EXE_NAME("D-LAN.Core.exe");
#else
   const QString CoreController::CORE_EXE_NAME("D-LAN.Core");
#endif

/**
  * Try to start the core as a service if it fails then try to launch it as a sub-process.
  */
CoreStatus CoreController::startCore(int port)
{
   QtServiceController controller(Common::Constants::SERVICE_NAME);
   if (!controller.isInstalled())
   {
      if (!QtServiceController::install(CORE_EXE_NAME))
         L_USER(QObject::tr("D-LAN Core cannot be installed as a service"));
   }

   bool isRunning = false;

   if (!(isRunning = controller.isRunning()))
   {
      QStringList arguments;
      if (port != -1)
         arguments << "--port" << QString::number(port);

      if (!(isRunning = controller.start(arguments)))
      {
         if (coreProcess.state() == QProcess::NotRunning)
         {
            coreProcess.start(QString("%1/%2 -e%3").arg(QCoreApplication::applicationDirPath()).arg(CORE_EXE_NAME).arg(port != -1 ? QString("") : QString(" --port %1").arg(port)));
            L_USER(QObject::tr("Core launched as subprocess"));
            return coreProcess.state() == QProcess::Starting || coreProcess.state() == QProcess::Running ? RUNNING_AS_SUB_PROCESS : NOT_RUNNING;
         }
      }
      else
         L_USER(QObject::tr("Core service launched"));
   }

   if (isRunning)
      return RUNNING_AS_SERVICE;
   return NOT_RUNNING;
}

void CoreController::stopCore()
{
   QtServiceController controller(Common::Constants::SERVICE_NAME);
   if (controller.isRunning())
      controller.stop();

   if (coreProcess.state() == QProcess::Running)
   {
      coreProcess.write("quit\n");
      coreProcess.waitForFinished(2000);
   }
}

QProcess CoreController::coreProcess;
