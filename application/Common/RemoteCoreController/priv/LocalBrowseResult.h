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

#include <QtCore>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <ILocalBrowseResult.h>

namespace RCC
{
   class InternalCoreConnection;

   class LocalBrowseResult : public ILocalBrowseResult
   {
      Q_OBJECT
   public:
      LocalBrowseResult(InternalCoreConnection* coreConnection, const QString& path, int socketTimeout);
      void start();

   private slots:
      void browseResult(const Protos::GUI::LocalBrowseResult& browseResult);

   private:
      InternalCoreConnection* coreConnection;
      Protos::GUI::LocalBrowse browseMessage;
   };
}
