#ifndef GUI_SEARCHUTILS_H
#define GUI_SEARCHUTILS_H

#include <QString>
#include <QMetaType>

#include <Protos/common.pb.h>

#include <Common/KnownExtensions.h>

namespace GUI
{
   struct SearchType
   {
      enum class EntryType
      {
         ALL = 0,
         FILES_ONLY = 1,
         DIRS_ONLY = 2,
         FILES_BY_EXTENSION = 3,
      };

      SearchType() : entryType(EntryType::ALL) {}
      SearchType(EntryType entryType) : entryType(entryType) {}
      SearchType(Common::ExtensionCategory extensionCategory) : entryType(EntryType::FILES_BY_EXTENSION), extensionCategory(extensionCategory) {}

      EntryType entryType;
      Common::ExtensionCategory extensionCategory;
   };

   class SearchUtils
   {
   public:
      static QString getSearchTypeText(SearchType searchType, bool withAllExtensions = true);
      static QString getExtensionText(Common::ExtensionCategory extension, bool withAllExtensions = true);
      static QString getFindPatternSummary(const Protos::Common::FindPattern& findPattern, bool local = false);
      static QString getFindPatternWindowTitle(const Protos::Common::FindPattern& findPattern);
   };
}

Q_DECLARE_METATYPE(GUI::SearchType)

#endif
