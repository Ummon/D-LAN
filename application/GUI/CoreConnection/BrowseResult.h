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
  
#ifndef GUI_BROWSERESULT_H
#define GUI_BROWSERESULT_H

#include <QtCore>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <CoreConnection/IBrowseResult.h>

namespace GUI
{
   class CoreConnection;

   class BrowseResult : public IBrowseResult
   {
      Q_OBJECT
   public:
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID);
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entry& entry);
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots = true);
      void start();
      void setTag(quint64 tag);

   private slots:
      void browseResult(const Protos::GUI::BrowseResult& browseResult);

   private:
      void init(CoreConnection* coreConnection);

      CoreConnection* coreConnection;
      const Common::Hash peerID;
      Protos::GUI::Browse browseMessage;
      quint64 tag;
   };
}

#endif
