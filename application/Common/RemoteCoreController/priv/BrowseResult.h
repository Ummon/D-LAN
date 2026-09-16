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

#include <QSharedPointer>
#include <QPointer>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <IBrowseResult.h>

namespace RCC
{
   class InternalCoreConnection;

   class BrowseResult : public IBrowseResult, public QEnableSharedFromThis<BrowseResult>
   {
      Q_OBJECT
   public:
      BrowseResult(InternalCoreConnection* coreConnection, const Common::Hash& peerID, int socketTimeout);
      BrowseResult(InternalCoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entry& entry, int socketTimeout);
      BrowseResult(InternalCoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots, int socketTimeout);
      void start();

   private slots:
      void browseResult(const Protos::GUI::BrowseResult& browseResult);

   private:
      void init(InternalCoreConnection* coreConnection);

      QPointer<InternalCoreConnection> coreConnection;
      bool started = false;
      const Common::Hash peerID;
      Protos::GUI::Browse browseMessage;

      const quint64 tag;
      bool waitingForResult = false;
   };
}
