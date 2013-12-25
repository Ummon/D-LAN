#include <Search/SearchUtils.h>
using namespace GUI;

#include <QObject>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/KnownExtensions.h>

QString SearchUtils::getSearchTypeText(SearchType searchType, bool withAllExtensions)
{
   switch (searchType.entryType)
   {
   case SearchType::EntryType::ALL:
      return QObject::tr("All");

   case SearchType::EntryType::DIRS_ONLY:
      return QObject::tr("Directories");

   case SearchType::EntryType::FILES_ONLY:
      return QObject::tr("Files");

   case SearchType::EntryType::FILES_BY_EXTENSION:
   default:
      return getExtensionText(searchType.extensionCategory, withAllExtensions);
   }
}

QString SearchUtils::getExtensionText(Common::ExtensionCategory extension, bool withAllExtensions)
{
   QString result;

   switch (extension)
   {
   case Common::ExtensionCategory::AUDIO:
      result.append(QObject::tr("Audio"));
      break;

   case Common::ExtensionCategory::VIDEO:
      result.append(QObject::tr("Video"));
      break;

   case Common::ExtensionCategory::COMPRESSED:
      result.append(QObject::tr("Compressed"));
      break;

   case Common::ExtensionCategory::DOCUMENT:
      result.append(QObject::tr("Document"));
      break;

   case Common::ExtensionCategory::PICTURE:
      result.append(QObject::tr("Picture"));
      break;

   case Common::ExtensionCategory::SUBTITLE:
      result.append(QObject::tr("Subtitle"));
      break;

   case Common::ExtensionCategory::EXECUTABLE:
      result.append(QObject::tr("Executable"));
      break;

   case Common::ExtensionCategory::MEDIA_ARCHIVE:
      result.append(QObject::tr("Media archive"));
      break;
   }

   if (withAllExtensions)
   {
      result.append(": [");

      bool begining = true;
      foreach (QString e, Common::KnownExtensions::getExtensions(extension))
      {
         if (begining)
            begining = false;
         else
            result.append(", ");

         result.append(e);
      }

      result.append("]");
   }

   return result;
}

QString SearchUtils::getFindPatternSummary(const Protos::Common::FindPattern& findPattern, bool local)
{
   const QString SEPARATOR(" / ");
   QString result;

   const QString pattern = Common::ProtoHelper::getStr(findPattern, &Protos::Common::FindPattern::pattern);
   if (!pattern.isEmpty())
      result.append(pattern);

   if (findPattern.extension_filter_size() > 0)
   {
      try
      {
         Common::ExtensionCategory cat = Common::KnownExtensions::getCategoryFrom(Common::ProtoHelper::getRepeatedStr(findPattern, &Protos::Common::FindPattern::extension_filter, 0));
         if (!result.isEmpty())
            result += SEPARATOR;
         result += getSearchTypeText(cat, false);
      }
      catch (const Common::CategoryNotFoundException&)
      {
      }
   }
   else if (findPattern.category() != Protos::Common::FindPattern::FILE_DIR)
   {
      if (!result.isEmpty())
         result += SEPARATOR;

      result += getSearchTypeText(static_cast<SearchType::EntryType>(findPattern.category()));
   }

   if (findPattern.min_size() > 0 || findPattern.max_size() > 0)
   {
      if (!result.isEmpty())
         result += SEPARATOR;

      if (findPattern.min_size() > 0)
      {
         result += Common::Global::formatByteSize(findPattern.min_size());
         result += " ";
      }

      result += "-> ";

      if (findPattern.max_size() > 0)
         result += Common::Global::formatByteSize(findPattern.max_size());
   }

   if (local)
   {
      if (!result.isEmpty())
         result += SEPARATOR;

      result += QObject::tr("Local");
   }

   return result;
}

QString SearchUtils::getFindPatternWindowTitle(const Protos::Common::FindPattern& findPattern)
{
   const QString pattern = Common::ProtoHelper::getStr(findPattern, &Protos::Common::FindPattern::pattern);
   if (!pattern.isEmpty())
      return pattern;

   if (findPattern.extension_filter_size() > 0)
   {
      try
      {
         Common::ExtensionCategory cat = Common::KnownExtensions::getCategoryFrom(Common::ProtoHelper::getRepeatedStr(findPattern, &Protos::Common::FindPattern::extension_filter, 0));
         return getExtensionText(cat, false);
      }
      catch (const Common::CategoryNotFoundException&)
      {
      }
   }

   if (findPattern.min_size() > 0 || findPattern.max_size() > 0)
   {
      QString result;

      if (findPattern.min_size() > 0)
      {
         result += Common::Global::formatByteSize(findPattern.min_size());
         result += " ";
      }

      result += "-> ";

      if (findPattern.max_size() > 0)
         result += Common::Global::formatByteSize(findPattern.max_size());

      return result;
   }

   return QString();
}
