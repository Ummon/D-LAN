#include <priv/Cache/DataWriter.h>
using namespace FM;

#include <Common/Settings.h>

DataWriter::DataWriter(Chunk& chunk)
   : chunk(chunk)
{
   this->chunk.newDataWriterCreated();
}

DataWriter::~DataWriter()
{
   this->chunk.dataWriterDeleted();
}

bool DataWriter::write(const char* buffer, int nbBytes)
{
   return this->chunk.write(buffer, nbBytes);
}
