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

#include <IconProvider.h>

#import <AppKit/AppKit.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>

#include <QImage>
#include <QPixmap>

using namespace GUI;

QIcon IconProvider::getIconNative(const QString& type)
{
   @autoreleasepool
   {
      // Resolve the extension alone: entries may belong to another peer and
      // must not depend on local files or their custom icons.
      UTType* contentType = [UTType typeWithFilenameExtension:type.mid(1).toNSString()];
      if (!contentType || contentType.dynamic)
         return IconProvider::iconProvider.icon(QFileIconProvider::File);

      NSImage* nativeIcon = [[NSWorkspace sharedWorkspace] iconForContentType:contentType];
      QIcon icon;
      // Include larger representations so Qt can select sharp Retina pixmaps.
      for (const int size : {16, 22, 32, 48, 64, 128, 256})
      {
         NSRect rect = NSMakeRect(0, 0, size, size);
         CGImageRef nativeImage = [nativeIcon CGImageForProposedRect:&rect context:nil hints:nil];
         if (!nativeImage)
            continue;

         QImage image(size, size, QImage::Format_RGBA8888_Premultiplied);
         image.fill(Qt::transparent);
         CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
         CGContextRef context = CGBitmapContextCreate(image.bits(), size, size, 8, image.bytesPerLine(),
            colorSpace, CGBitmapInfo(kCGImageAlphaPremultipliedLast) | kCGBitmapByteOrder32Big);
         CGColorSpaceRelease(colorSpace);
         if (!context)
            continue;
         CGContextSetInterpolationQuality(context, kCGInterpolationHigh);
         CGContextDrawImage(context, CGRectMake(0, 0, size, size), nativeImage);
         CGContextRelease(context);
         icon.addPixmap(QPixmap::fromImage(image));
      }
      return icon.isNull() ? IconProvider::iconProvider.icon(QFileIconProvider::File) : icon;
   }
}
