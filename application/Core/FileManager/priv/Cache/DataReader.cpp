#include <priv/Cache/DataReader.h>
using namespace FM;

#include <Common/Settings.h>

DataReader::DataReader(Chunk& chunk)
   : chunk(chunk)
{
   this->chunk.newDataReaderCreated();
}

DataReader::~DataReader()
{
   this->chunk.dataReaderDeleted();
}

quint64 DataReader::read(char* buffer, uint offset)
{
   return this->chunk.read(buffer, offset);
}
