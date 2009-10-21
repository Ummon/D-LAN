#include <priv/Cache/DataWriter.h>
using namespace FM;

DataWriter::DataWriter(Chunk& chunk)
   : chunk(chunk)
{
   this->chunk.getFile().newDataWriterCreated();
}

DataWriter::~DataWriter()
{
   this->chunk.getFile().dataWriterDeleted();
}

bool DataWriter::write(const QByteArray& buffer, uint offset)
{
   return this->chunk.write(buffer, offset);
}
