#ifndef DOWNLOADER_H
#define DOWNLOADER_H

#include <QtCore/QThread>

#include <Chunk.h>

class Downloader : public QThread
{
   Q_OBJECT
public:
   Downloader();

   void setChunk(Chunk* chunk);

   static void fillBuffer(QByteArray& buffer);   
   static const uint bufferSize; ///< The buffer size, some kilobytes.
   
protected:
   void run();
   
private :
   Chunk* chunk;   
};

#endif
