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

#include <QProcessEnvironment>

#include <priv/Log.h>

#ifdef Q_OS_WIN32
   const QString CoreController::CORE_EXE_NAME("D-LAN.Core.exe");
#else
   const QString CoreController::CORE_EXE_NAME("D-LAN.Core");
#endif

const int CoreController::TIMEOUT_SUBPROCESS_WAIT_FOR_STARTED(2000); // 2s.

CoreController::CoreController() :
   controller(Common::Constants::SERVICE_NAME)
{
   connect(&this->coreProcess, SIGNAL(stateChanged(QProcess::ProcessState)), this, SIGNAL(statusChanged()));
}

/**
  * Try to start the core as a service if it fails then try to launch it as a sub-process.
  */
void CoreController::startCore(int port)
{
   if (!this->controller.isInstalled())
   {
      if (!QtServiceController::install(CORE_EXE_NAME))
         L_WARN(QObject::tr("D-LAN Core cannot be installed as a service"));
   }

   if (!this->controller.isRunning())
   {
      QStringList arguments;
      if (port != -1)
         arguments << "--port" << QString::number(port);

      if (!this->controller.start(arguments))
      {
         if (this->coreProcess.state() == QProcess::NotRunning)
         {
            this->coreProcess.start(QString("\"%1/%2\" -e%3").arg(QCoreApplication::applicationDirPath()).arg(CORE_EXE_NAME).arg(port != -1 ? QString("") : QString(" --port %1").arg(port)));
            L_USER(QObject::tr("Core launched as subprocess"));
            this->coreProcess.waitForStarted(TIMEOUT_SUBPROCESS_WAIT_FOR_STARTED);
         }
      }
      else
      {
         L_USER(QObject::tr("Core service launched"));
         emit statusChanged();
      }
   }
}

void CoreController::stopCore()
{
   if (this->controller.isRunning())
      this->controller.stop();

   if (this->coreProcess.state() == QProcess::Running)
   {
      this->coreProcess.write("quit\n");
      this->coreProcess.waitForFinished(2000);
   }

   this->coreProcess.kill();
}

CoreStatus CoreController::getStatus() const
{
   return this->controller.isRunning() ? RUNNING_AS_SERVICE : (this->coreProcess.state() == QProcess::Running || this->coreProcess.state() == QProcess::Starting ? RUNNING_AS_SUB_PROCESS : NOT_RUNNING);
}
