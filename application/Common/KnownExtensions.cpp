#include <Common/KnownExtensions.h>
using namespace Common;

bool KnownExtensions::exists(const QString& extension)
{
   return extensions.find(extension) != extensions.constEnd();
}

int KnownExtensions::nbCategory()
{
   return extensionsByCategory.length();
}

QList<QString> KnownExtensions::getExtension(ExtensionCategory cat)
{
   int i = (int)cat;
   if (i < nbCategory())
      return extensionsByCategory[i];
   else return QList<QString>();
}

int KnownExtensions::getBeginingExtension(const QString& filename)
{
   int i = 0;
   while ((i = filename.indexOf('.', i + 1)) != -1)
   {
      if (i == filename.length() - 1)
         break;

      if (exists(filename.right(filename.length() - 1 - i)))
         return i + 1;
   }

   return -1;
}

QString KnownExtensions::removeExtension(const QString& filename)
{
   int i = getBeginingExtension(filename);
   if (i != -1)
      return filename.left(i - 1);
   else
      return filename;
}

QString KnownExtensions::getExtension(const QString& filename)
{
   int i = getBeginingExtension(filename);
   if (i != -1)
      return filename.right(filename.length() - i);
   else
      return QString();
}

void KnownExtensions::add(ExtensionCategory cat, const QString& extension)
{
   extensions.insert(extension, cat);

   int i = (int)cat;
   while (i >= extensionsByCategory.length())
      extensionsByCategory << QList<QString>();
   extensionsByCategory[i] << extension;
}

KnownExtensions::Init::Init()
{
   KnownExtensions::add(AUDIO, "mp3");
   KnownExtensions::add(AUDIO, "mp2");
   KnownExtensions::add(AUDIO, "mid");
   KnownExtensions::add(AUDIO, "wav");
   KnownExtensions::add(AUDIO, "ogg");
   KnownExtensions::add(AUDIO, "wma");
   KnownExtensions::add(AUDIO, "aac");
   KnownExtensions::add(AUDIO, "flac");
   KnownExtensions::add(AUDIO, "au");
   KnownExtensions::add(AUDIO, "aiff");

   KnownExtensions::add(VIDEO, "mpg");
   KnownExtensions::add(VIDEO, "mpeg");
   KnownExtensions::add(VIDEO, "mov");
   KnownExtensions::add(VIDEO, "m4v");
   KnownExtensions::add(VIDEO, "asf");
   KnownExtensions::add(VIDEO, "avi");
   KnownExtensions::add(VIDEO, "pxp");
   KnownExtensions::add(VIDEO, "wmv");
   KnownExtensions::add(VIDEO, "ogm");
   KnownExtensions::add(VIDEO, "mlv");
   KnownExtensions::add(VIDEO, "rm");
   KnownExtensions::add(VIDEO, "rmvb");
   KnownExtensions::add(VIDEO, "divx");

   KnownExtensions::add(COMPRESSED, "zip");
   KnownExtensions::add(COMPRESSED, "ace");
   KnownExtensions::add(COMPRESSED, "rar");
   KnownExtensions::add(COMPRESSED, "tar.gz");
   KnownExtensions::add(COMPRESSED, "tar.gz2");
   KnownExtensions::add(COMPRESSED, "tar.gz");

   KnownExtensions::add(DOCUMENT, "html");
   KnownExtensions::add(DOCUMENT, "htm");
   KnownExtensions::add(DOCUMENT, "doc");
   KnownExtensions::add(DOCUMENT, "txt");
   KnownExtensions::add(DOCUMENT, "nfo");
   KnownExtensions::add(DOCUMENT, "odt");

   KnownExtensions::add(PICTURE, "jpg");
   KnownExtensions::add(PICTURE, "jpeg");
   KnownExtensions::add(PICTURE, "gif");
   KnownExtensions::add(PICTURE, "png");
   KnownExtensions::add(PICTURE, "eps");
   KnownExtensions::add(PICTURE, "img");
   KnownExtensions::add(PICTURE, "pct");
   KnownExtensions::add(PICTURE, "psp");
   KnownExtensions::add(PICTURE, "pic");
   KnownExtensions::add(PICTURE, "tif");
   KnownExtensions::add(PICTURE, "rle");
   KnownExtensions::add(PICTURE, "bmp");
   KnownExtensions::add(PICTURE, "pcx");

   KnownExtensions::add(EXECUTABLE, "exe");
   KnownExtensions::add(EXECUTABLE, "msi");
}

QHash<QString, ExtensionCategory> KnownExtensions::extensions;
QList<QList<QString>> KnownExtensions::extensionsByCategory;

KnownExtensions::Init KnownExtensions::initializer;


