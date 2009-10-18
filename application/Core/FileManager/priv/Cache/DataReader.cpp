#include <priv/Cache/DataReader.h>
using namespace FileManager;

DataReader::DataReader(Chunk& chunk)
   : chunk(chunk)
{
   this->chunk.getFile().newDataReaderCreated();
}

DataReader::~DataReader()
{
   this->chunk.getFile().dataReaderDeleted();
}

qint64 DataReader::read(QByteArray& buffer, uint offset)
{
   return this->chunk.read(buffer, offset);
}
