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

#include <Protos/core_protocol.pb.h>

#include <IGetChunksResult.h>
#include <priv/Result.h>

namespace PM
{
   class GetChunksResult : public Result<IGetChunksResult, Protos::Core::GetChunks>
   {
   public:
      GetChunksResult(const Protos::Core::GetChunks& chunks, QSharedPointer<PeerMessageSocket> socket);
      void setStatus(bool closeTheSocket) override;

   private:
      void newMessage(const Common::Message& message) override;
      void socketClosed() override;

      bool streaming = false; // The socket has been given to the caller to read the data of the chunks.
   };
}
