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
  
#ifndef CORE_CORE_H
#define CORE_CORE_H

#include <QObject>
#include <QLocale>
#include <QSharedPointer>
#include <QTranslator>

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
   class Core : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Core(bool resetSettings, QLocale locale);
      void start();

   public slots:
      void setLanguage(QLocale locale, bool load = true);

   private:
      void checkSettingsIntegrity();

      template <typename T>
      void checkSetting(const QString& name, T min, T max, bool mustBeAPowerOf32 = false);

      // This objet will be the last destroyed.
      struct Cleaner { ~Cleaner() {
         SETTINGS.free();
         google::protobuf::ShutdownProtobufLibrary();
      } } cleaner;

      LOG_INIT_H("Core");

      QTranslator translator;

      struct TheLastWords { ~TheLastWords() { L_USER(QObject::tr("Shutdown")); } } theLastWords;

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
void Core::checkSetting(const QString& name, T min, T max, bool mustBeAPowerOf32)
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
