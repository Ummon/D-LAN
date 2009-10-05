#ifndef FILEMANAGER_ICHUNK_H
#define FILEMANAGER_ICHUNK_H

#include <QtGlobal>
#include <QByteArray>

#include <Common/Hash.h>

namespace FileManager
{
   class IChunk
   {
   public:
      virtual void read(const quint32& offset, QByteArray& data) = 0;
      virtual void write(const quint32& offset, const QByteArray& data) = 0;
      virtual Common::Hash getHash() = 0;
   };
}
#endif
