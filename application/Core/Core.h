#ifndef CORE_CORE_H
#define CORE_CORE_H

#include <QSharedPointer>
#include <QObject>

namespace LM { class ILogger; }
namespace FM { class IFileManager; }
namespace RCM { class IRemoteControlManager; }
namespace PM { class IPeerManager; }
namespace NL { class INetworkListener; }
namespace DL { class IDownloadManager; }
namespace UM { class IUploadManager; }
namespace DM { class IDownloadManager; }

namespace Core
{
   class Core : public QObject
   {
   Q_OBJECT

   public:
      Core();
      virtual ~Core();

   private:
      QSharedPointer<LM::ILogger> logger;
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<NL::INetworkListener> networkListener;
      QSharedPointer<PM::IPeerManager> peerManager;

      RCM::IRemoteControlManager* remoteControlManager;
      DM::IDownloadManager* downloadManager;
      UM::IUploadManager* uploadManager;
   };
}
#endif
