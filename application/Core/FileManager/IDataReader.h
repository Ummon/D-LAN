#ifndef FILEMANAGER_IDATAREADER_H
#define FILEMANAGER_IDATAREADER_H

#include <QByteArray>

namespace FileManager
{
   class IDataReader
   {
   public:
      virtual ~IDataReader() {}
      virtual qint64 read(QByteArray& buffer, uint offset) = 0;
   };
}

#endif
