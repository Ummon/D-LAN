#ifndef NETWORKLISTENER_NETWORKLISTENER_H
#define NETWORKLISTENER_NETWORKLISTENER_H

#include <QObject>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/DownloadManager/IDownloadManager.h>

#include <INetworkListener.h>
#include <ISearch.h>
#include <priv/UDPListener.h>
#include <priv/TCPListener.h>
#include <priv/Chat.h>

namespace NL
{
   class NetworkListener : public QObject, public INetworkListener
   {
      Q_OBJECT
   public:
      NetworkListener(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<DM::IDownloadManager> downloadManager
      );

      ~NetworkListener();

      IChat& getChat();
      QSharedPointer<ISearch> newSearch();

   private:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;

      TCPListener tCPListener;
      UDPListener uDPListener;
      Chat chat;
   };
}
#endif
