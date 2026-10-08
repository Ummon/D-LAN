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

const int CoreController::TIMEOUT_SUBPROCESS_WAIT_FOR_STARTED(3000); // 3s.
const int CoreController::TIMEOUT_SUBPROCESS_WAIT_FOR_STOPPED(5000); // 5s.

CoreController::CoreController() :
#ifdef Q_OS_LINUX
   controller(Common::Constants::SYSTEMD_UNIT_NAME)
#else
   controller(Common::Constants::SERVICE_NAME)
#endif
{
   this->setProgramPath();

   connect(&this->coreProcess, &QProcess::stateChanged, this, &CoreController::statusChanged);
}

CoreController::~CoreController()
{
   // QProcess can emit stateChanged from its destructor, after the service
   // controller has been destroyed. Do not expose a partially destroyed owner.
   this->coreProcess.disconnect(this);
}

void CoreController::setCoreExecutableDirectory(const QString& dir)
{
   this->coreDirectory = dir;
   this->setProgramPath();
}

void CoreController::setAutoStart(bool autoStart)
{
   this->autoStart = autoStart;
}

bool CoreController::isAutoStart() const
{
   return this->autoStart;
}

/**
  * Try to start the core as a service if it fails then try to launch it as a sub-process.
  * The service is never installed here, see the '-i' argument of the Core.
  * When compiling with the DEBUG directive only the sub-process will be launched, not the service.
  * @param port The port the core will listen to for managing it with a client (GUI for instance).
  * A systemd service can't be given a port when it's started, it listens to the one defined in its settings.
  */
void CoreController::startCore(int port)
{
   // A running subprocess may still be initializing its TCP listener. Retrying
   // the connection must not attempt service installation or launch it again.
   if (this->coreProcess.state() != QProcess::NotRunning)
      return;

   const bool debug =
      #if defined(DEBUG)
         true;
      #else
         false;
      #endif

   if (!this->controller.isRunning())
   {
      QStringList arguments;
      if (port != -1)
         arguments << "--port" << QString::number(port);

      if (debug || !this->controller.start(arguments))
      {
         this->coreProcess.setArguments(arguments);
         this->coreProcess.start();

         if (this->coreProcess.waitForStarted(TIMEOUT_SUBPROCESS_WAIT_FOR_STARTED))
            L_USER(QObject::tr("Core launched as subprocess"));
         else
            L_WARN(QObject::tr("Unable to launch the Core as subprocess"));
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
      if (!this->coreProcess.waitForFinished(TIMEOUT_SUBPROCESS_WAIT_FOR_STOPPED))
         L_WARN("Core doesn't stopped properly");
   }

   this->coreProcess.kill();
}

CoreStatus CoreController::getStatus() const
{
   if (this->controller.isRunning())
      return RUNNING_AS_SERVICE;
   return this->coreProcess.state() != QProcess::NotRunning ? RUNNING_AS_SUB_PROCESS : NOT_RUNNING;
}

void CoreController::setProgramPath()
{
   this->coreProcess.setProgram(
      QString("%1/%2").arg(
         this->coreDirectory.isNull() ? QCoreApplication::applicationDirPath() : this->coreDirectory,
         CORE_EXE_NAME
      )
   );
}
