#ifndef FILEMANAGER_IDATAWRITER_H
#define FILEMANAGER_IDATAWRITER_H

#include <QByteArray>

namespace FileManager
{
   class IDataWriter
   {
   public:
      virtual ~IDataWriter() {}
      virtual bool write(const QByteArray& buffer, uint offset) = 0;
   };
}

#endif
