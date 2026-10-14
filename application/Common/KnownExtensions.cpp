#include <Common/KnownExtensions.h>
#include <QStringView>
#include <QVarLengthArray>
using namespace Common;

bool KnownExtensions::exists(const QString& extension)
{
   return extensions.find(extension.toLower()) != extensions.constEnd();
}

int KnownExtensions::nbCategory()
{
   return extensionsByCategory.length();
}

QList<QString> KnownExtensions::getExtensions(ExtensionCategory cat)
{
   int i = (int)cat;
   if (i < nbCategory())
      return extensionsByCategory[i];
   else return QList<QString>();
}

ExtensionCategory KnownExtensions::getCategoryFrom(const QString& extension)
{
   auto i = extensions.find(extension.toLower());
   if (i == extensions.end())
      throw CategoryNotFoundException();
   else
      return *i;
}

int KnownExtensions::getBeginningExtension(const QString& filename)
{
   return findExtension(filename).beginning;
}

QString KnownExtensions::removeExtension(const QString& filename)
{
   int i = getBeginningExtension(filename);
   if (i != -1)
      return filename.left(i - 1);
   else
      return filename;
}

/**
  * Returns an existing extension with the same case a the filename.
  */
QString KnownExtensions::getExtension(const QString& filename)
{
   int i = getBeginningExtension(filename);
   if (i != -1)
      return filename.right(filename.length() - i);
   else
      return QString();
}

/**
  * Returns an existing extension in lower case, as it's known: the same value as 'getExtension(filename).toLower()'
  * without the memory allocations, see 'findExtension(..)'.
  */
QString KnownExtensions::getNormalizedExtension(const QString& filename)
{
   const QString* extension = findExtension(filename).normalized;
   return extension ? *extension : QString();
}

KnownExtensions::Extension KnownExtensions::findExtension(const QString& filename)
{
   // Only the final maxExtensionLength characters can be a known extension.
   // As before, a leading dot alone does not introduce an extension.
   qsizetype i = filename.indexOf('.', qMax(qsizetype(1), filename.size() - maxExtensionLength - 1));
   if (i == -1 || i == filename.size() - 1)
      return { -1, nullptr };

   // The end of the filename in lower case. Usually it's only made of ASCII characters: they are lowered
   // one by one in a local buffer, without any memory allocation.
   const QStringView end = QStringView(filename).sliced(i + 1);
   QVarLengthArray<QChar, 16> asciiNormalized;
   QString normalized;
   QStringView suffix;
   for (const QChar c : end)
   {
      if (c.unicode() >= 0x80)
      {
         normalized = end.toString().toLower();
         suffix = normalized;
         break;
      }
      asciiNormalized.append(c.toLower());
   }
   if (suffix.isNull())
      suffix = QStringView(asciiNormalized.constData(), asciiNormalized.size());

   while (true)
   {
      // This temporary QString borrows the storage of the suffix without allocating.
      const auto extension = extensions.constFind(QString::fromRawData(suffix.data(), suffix.size()));
      if (extension != extensions.constEnd())
         return { static_cast<int>(i + 1), &extension.key() };

      i = filename.indexOf('.', i + 1);
      if (i == -1 || i == filename.size() - 1)
         break;

      // Lowercasing can expand Unicode characters, so advance through the
      // original and normalized strings independently to preserve the index.
      suffix = suffix.sliced(suffix.indexOf('.') + 1);
   }

   return { -1, nullptr };
}

void KnownExtensions::add(ExtensionCategory cat, const QString& extension)
{
   extensions.insert(extension, cat);
   maxExtensionLength = qMax(maxExtensionLength, extension.size());

   int i = (int)cat;
   while (i >= extensionsByCategory.length())
      extensionsByCategory << QList<QString>();
   extensionsByCategory[i] << extension;
}

KnownExtensions::Init::Init()
{
   KnownExtensions::add(ExtensionCategory::AUDIO, "mp3");
   KnownExtensions::add(ExtensionCategory::AUDIO, "mp2");
   KnownExtensions::add(ExtensionCategory::AUDIO, "mid");
   KnownExtensions::add(ExtensionCategory::AUDIO, "wav");
   KnownExtensions::add(ExtensionCategory::AUDIO, "ogg");
   KnownExtensions::add(ExtensionCategory::AUDIO, "wma");
   KnownExtensions::add(ExtensionCategory::AUDIO, "aac");
   KnownExtensions::add(ExtensionCategory::AUDIO, "flac");
   KnownExtensions::add(ExtensionCategory::AUDIO, "la");
   KnownExtensions::add(ExtensionCategory::AUDIO, "au");
   KnownExtensions::add(ExtensionCategory::AUDIO, "aif");
   KnownExtensions::add(ExtensionCategory::AUDIO, "aifc");
   KnownExtensions::add(ExtensionCategory::AUDIO, "aiff");

   KnownExtensions::add(ExtensionCategory::VIDEO, "mpg");
   KnownExtensions::add(ExtensionCategory::VIDEO, "mpeg");
   KnownExtensions::add(ExtensionCategory::VIDEO, "mp4");
   KnownExtensions::add(ExtensionCategory::VIDEO, "m4v");
   KnownExtensions::add(ExtensionCategory::VIDEO, "m4p");
   KnownExtensions::add(ExtensionCategory::VIDEO, "mov");
   KnownExtensions::add(ExtensionCategory::VIDEO, "asf");
   KnownExtensions::add(ExtensionCategory::VIDEO, "avi");
   KnownExtensions::add(ExtensionCategory::VIDEO, "pxp");
   KnownExtensions::add(ExtensionCategory::VIDEO, "wmv");
   KnownExtensions::add(ExtensionCategory::VIDEO, "ogm");
   KnownExtensions::add(ExtensionCategory::VIDEO, "ogv");
   KnownExtensions::add(ExtensionCategory::VIDEO, "mkv");
   KnownExtensions::add(ExtensionCategory::VIDEO, "rm");
   KnownExtensions::add(ExtensionCategory::VIDEO, "rmvb");
   KnownExtensions::add(ExtensionCategory::VIDEO, "divx");
   KnownExtensions::add(ExtensionCategory::VIDEO, "webm");

   KnownExtensions::add(ExtensionCategory::COMPRESSED, "zip");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "7z");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "ace");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "rar");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "bzip2");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "tar.gz");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "tar.gz2");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "tar.gz");
   KnownExtensions::add(ExtensionCategory::COMPRESSED, "cab");

   KnownExtensions::add(ExtensionCategory::DOCUMENT, "html");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "htm");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "xhtml");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "doc");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "docx");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "dot");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "dotx");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "epub");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "mobi");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "txt");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "nfo");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "odm");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "odt");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "ott");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "pdf");
   KnownExtensions::add(ExtensionCategory::DOCUMENT, "rtf");

   KnownExtensions::add(ExtensionCategory::PICTURE, "jpg");
   KnownExtensions::add(ExtensionCategory::PICTURE, "jpeg");
   KnownExtensions::add(ExtensionCategory::PICTURE, "jxl");
   KnownExtensions::add(ExtensionCategory::PICTURE, "webp");
   KnownExtensions::add(ExtensionCategory::PICTURE, "avif");
   KnownExtensions::add(ExtensionCategory::PICTURE, "gif");
   KnownExtensions::add(ExtensionCategory::PICTURE, "png");
   KnownExtensions::add(ExtensionCategory::PICTURE, "eps");
   KnownExtensions::add(ExtensionCategory::PICTURE, "img");
   KnownExtensions::add(ExtensionCategory::PICTURE, "ico");
   KnownExtensions::add(ExtensionCategory::PICTURE, "pct");
   KnownExtensions::add(ExtensionCategory::PICTURE, "psp");
   KnownExtensions::add(ExtensionCategory::PICTURE, "psd");
   KnownExtensions::add(ExtensionCategory::PICTURE, "xcf"); // GIMP.
   KnownExtensions::add(ExtensionCategory::PICTURE, "pic");
   KnownExtensions::add(ExtensionCategory::PICTURE, "tif");
   KnownExtensions::add(ExtensionCategory::PICTURE, "tiff");
   KnownExtensions::add(ExtensionCategory::PICTURE, "rle");
   KnownExtensions::add(ExtensionCategory::PICTURE, "bmp");
   KnownExtensions::add(ExtensionCategory::PICTURE, "pcx");

   KnownExtensions::add(ExtensionCategory::SUBTITLE, "sub");
   KnownExtensions::add(ExtensionCategory::SUBTITLE, "srt");
   KnownExtensions::add(ExtensionCategory::SUBTITLE, "pcx");

   KnownExtensions::add(ExtensionCategory::EXECUTABLE, "exe");
   KnownExtensions::add(ExtensionCategory::EXECUTABLE, "msi");

   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "iso");
   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "nrg");
   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "sdi");
   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "mds");
   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "dmg");
   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "cdi");
   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "cue");
   KnownExtensions::add(ExtensionCategory::MEDIA_ARCHIVE, "cif");
}

QHash<QString, ExtensionCategory> KnownExtensions::extensions;
QList<QList<QString>> KnownExtensions::extensionsByCategory;
qsizetype KnownExtensions::maxExtensionLength = 0;

KnownExtensions::Init KnownExtensions::initializer;

