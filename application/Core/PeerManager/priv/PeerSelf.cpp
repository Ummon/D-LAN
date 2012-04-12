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
Hash PeerSelf::loadID()
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
