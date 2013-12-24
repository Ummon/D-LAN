#ifndef GUI_SEARCHUTILS_H
#define GUI_SEARCHUTILS_H

#include <QString>

#include <Protos/common.pb.h>

#include <Common/KnownExtensions.h>

namespace GUI
{
   class SearchUtils
   {
   public:
      static QString getCategoryText(Protos::Common::FindPattern_Category category);
      static QString getExtensionText(Common::ExtensionCategory extension, bool withAllExtensions = true);
      static QString getFindPatternSummary(const Protos::Common::FindPattern& findPattern, bool local = false);
      static QString getFindPatternWindowTitle(const Protos::Common::FindPattern& findPattern);
   };
}

#endif
