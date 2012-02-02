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
  
#ifndef FILEMANAGER_IGETHASHESRESULT_H
#define FILEMANAGER_IGETHASHESRESULT_H

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

namespace FM
{
   class IGetHashesResult : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IGetHashesResult() {}
      virtual Protos::Core::GetHashesResult start() = 0;

   signals:
      /**
        * This signal must be connected as Qt::QueuedConnection!
        * If not, the connected slot may be called right after the 'start()' call and thus don't
        * give the caller the time to treat the 'start()' return value.
        */
      void nextHash(Common::Hash hash);
   };
}
#endif
