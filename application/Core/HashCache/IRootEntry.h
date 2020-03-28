#pragma once

#include <QString>

#include <Common/Hash.h>

#include <IEntry.h>

namespace HC
{
   class IRootEntry
   {
   public:
      virtual ~IRootEntry() {}

      virtual QString getPath() = 0;
      virtual Common::Hash getID() = 0;

      /**
        * It returns a file (IFile) or a directory (IDir).
        */
      virtual IEntry& getRoot() = 0;
   };
}
