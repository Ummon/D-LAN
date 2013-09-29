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
  
#ifndef RCC_ISENDCHATMESSAGERESULT_H
#define RCC_ISENDCHATMESSAGERESULT_H

#include <Protos/gui_protocol.pb.h>

#include <Common/Timeoutable.h>

namespace RCC
{
   class ISendChatMessageResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      ISendChatMessageResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~ISendChatMessageResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::GUI::ChatMessageResult&);
   };
}

#endif
