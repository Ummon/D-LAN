#ifndef CORE_CORE_H
#define CORE_CORE_H

#include <QSharedPointer>

#include <Common/Settings.h>
#include <Common/Uncopyable.h>

#include <FileManager/IFileManager.h>
#include <PeerManager/IPeerManager.h>
#include <UploadManager/IUploadManager.h>
#include <DownloadManager/IDownloadManager.h>
#include <NetworkListener/INetworkListener.h>
#include <RemoteControlManager/IRemoteControlManager.h>

#include <Log.h>

namespace CoreSpace
{
   // Better than the Arm.
   class Core : Common::Uncopyable
   {
   public:
      Core(const QString& settingsFileName = QString());
      void start();

   private:
      void checkSettingsIntegrity();

      template <typename T>
      void checkSetting(const QString name, T min, T max, bool mustBeAPowerOf32 = false);

      // This objet will be the last destroyed.
      struct ProtobufCleaner { ~ProtobufCleaner() { google::protobuf::ShutdownProtobufLibrary(); } } protobufCleaner;

   protected:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;
      QSharedPointer<NL::INetworkListener> networkListener;
      QSharedPointer<RCM::IRemoteControlManager> remoteControlManager;
   };
}


/***** Definitions *****/
using namespace CoreSpace;

template <typename T>
void Core::checkSetting(const QString name, T min, T max, bool mustBeAPowerOf32)
{
   T actualValue = SETTINGS.get<T>(name);
   T newValue;
   bool error = false;

   if (actualValue > max)
   {
      error = true;
      newValue = max;
   }
   else if (actualValue < min)
   {
      error = true;
      newValue = min;
   }

   if (error)
   {
      L_ERRO(
         QString("The value of the setting '%1' must be between %2 and %3. The actual value of %4 is modified to %5").
         arg(name).arg(min).arg(max).arg(actualValue).arg(newValue)
      );
      SETTINGS.set(name, newValue);
   }
}

#endif
