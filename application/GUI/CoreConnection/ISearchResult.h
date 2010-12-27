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
  
#ifndef GUI_ISEARCHRESULT_H
#define GUI_ISEARCHRESULT_H

#include <Protos/common.pb.h>

#include <Common/Timeoutable.h>

namespace GUI
{
   class ISearchResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      ISearchResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~ISearchResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Common::FindResult&);
   };
}

#endif
