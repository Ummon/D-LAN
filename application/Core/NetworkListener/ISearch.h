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
  
#pragma once

#include <QObject>
#include <QString>

#include <Protos/common.pb.h>

namespace NL
{
   class ISearch : public QObject
   {
      Q_OBJECT
   public:
      virtual ~ISearch() {}

      /**
        * Begin a new search. Only one successful launch is allowed per object.
        * @return A nonzero tag repeated in the results (see 'found'), or 0 if sending fails
        *         or the search was already launched. A failed send can be retried.
        */
      virtual quint64 search(const Protos::Common::FindPattern& findPattern) = 0;

      /**
        * @return ms elapsed since the successful launch, or -1 if no search has been launched.
        */
      virtual qint64 elapsed() = 0;

   signals:
      void found(const Protos::Common::FindResult& result);
   };
}
