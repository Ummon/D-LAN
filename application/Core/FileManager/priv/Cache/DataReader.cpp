#include <priv/Cache/DataReader.h>
using namespace FM;

DataReader::DataReader(Chunk& chunk)
   : chunk(chunk)
{
   this->chunk.newDataReaderCreated();
}

DataReader::~DataReader()
{
   this->chunk.dataReaderDeleted();
}

qint64 DataReader::read(QByteArray& buffer, uint offset)
{
   return this->chunk.read(buffer, offset);
}
