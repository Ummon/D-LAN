#pragma once

#include <IFileManager.h>
using namespace FM;

#include <functional>

#include <QSharedPointer>
#include <QFile>

#include <Common/Path.h>

class Utils
{
   public:
      static Protos::Common::Entry tryFindEntry(
         QSharedPointer<IFileManager> fileManager,
         Common::Path entryPath
      );

      static bool retry(int nbTries, int waitBetweenTries_ms, std::function<bool()> fun);

      static bool tryOpen(QFile& file, QIODeviceBase::OpenModeFlag flags);
};

