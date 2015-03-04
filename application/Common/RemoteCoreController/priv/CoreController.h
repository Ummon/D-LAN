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
  
#ifndef RCC_CORECONTROLLER_H
#define RCC_CORECONTROLLER_H

#include <QtServiceController>
#include <QProcess>

#include <Common/Constants.h>

#include <Types.h>

namespace RCC
{
   class CoreController : public QObject
   {
      static const QString CORE_EXE_NAME;
      static const int TIMEOUT_SUBPROCESS_WAIT_FOR_STARTED; // [ms];

      Q_OBJECT

   public:
      CoreController();

      void setCoreExecutableDirectory(const QString& dir);
      void startCore(int port = -1);
      void stopCore();

      CoreStatus getStatus() const;

   signals:
      void statusChanged();

   private:
      QProcess coreProcess; ///< Only used when unable to launch the core as a service.
      QtServiceController controller;
      QString coreDirectory;
   };
}

#endif
