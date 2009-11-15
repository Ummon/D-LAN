#include <priv/Cache/DataWriter.h>
using namespace FM;

DataWriter::DataWriter(Chunk& chunk)
   : chunk(chunk)
{
   this->chunk.newDataWriterCreated();
}

DataWriter::~DataWriter()
{
   this->chunk.dataWriterDeleted();
}

bool DataWriter::write(const QByteArray& buffer, uint offset)
{
   return this->chunk.write(buffer, offset);
}
