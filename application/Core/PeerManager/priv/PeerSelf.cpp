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
  
#include <priv/PeerSelf.h>
using namespace PM;

#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/Constants.h>

PM::PeerSelf::PeerSelf(PM::PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager) :
   Peer(peerManager, fileManager, loadID(), SETTINGS.get<QString>("nick"))
{
   this->IP = QHostAddress::LocalHost;
   this->port = SETTINGS.get<quint32>("unicast_base_port");
   this->alive = true;

   this->connectionPool.setIP(this->IP, this->port);

   L_USER(QString(tr("Our current ID: %1")).arg(this->ID.toStr()));
}

void PeerSelf::setNick(const QString& nick)
{
   if (nick.length() > MAX_NICK_LENGTH)
      this->nick = nick.left(MAX_NICK_LENGTH);
   else
      this->nick = nick;

   SETTINGS.set("nick", this->nick);

   if (!SETTINGS.save())
      L_ERRO("Unable to save settings");
}

/**
  * Load the peer ID from the settings or create a new one if it doesn't exist.
  */
Common::Hash PeerSelf::loadID()
{
   Common::Hash ID;
   if (!SETTINGS.isSet("peer_id") || SETTINGS.get<Common::Hash>("peer_id").isNull())
   {
      ID = Common::Hash::rand();
      SETTINGS.set("peer_id", ID);

      if (!SETTINGS.save())
         L_ERRO("Unable to save settings");
   }
   else
   {
      ID = SETTINGS.get<Common::Hash>("peer_id");
   }
   return ID;
}
