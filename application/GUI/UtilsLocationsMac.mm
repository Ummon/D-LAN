#include <Utils.h>

#import <AppKit/AppKit.h>

#include <QDesktopServices>
#include <QFileInfo>
#include <QUrl>

using namespace GUI;

void Utils::openLocation(const QString& path, QWidget* parent)
{
   Q_UNUSED(parent)
   if (path.isEmpty())
      return;

   const QFileInfo fileInfo(path);
   if (fileInfo.isDir() || !fileInfo.exists())
   {
      // A missing download cannot be selected; still open its containing folder.
      const QString directory = fileInfo.isDir() ? fileInfo.absoluteFilePath() : fileInfo.absolutePath();
      QDesktopServices::openUrl(QUrl::fromLocalFile(directory));
      return;
   }

   @autoreleasepool
   {
      // Use a file URL to preserve Unicode, spaces and URL-reserved characters.
      NSURL* url = QUrl::fromLocalFile(fileInfo.absoluteFilePath()).toNSURL();
      if (url)
         [[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:@[url]];
   }
}
