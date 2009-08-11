#ifndef DOWNLOADER_H
#define DOWNLOADER_H

#include <QtCore/QThread>

#include <Chunk.h>

/**
  * Will fill a chunk with downloaded data.
  */
class Downloader : public QThread
{
   Q_OBJECT
public:
   Downloader();
   
   /**
     * Defines a chunk, must be called prior 'run()'.
     * @param chunk The chunk.
     */
   void setChunk(Chunk* chunk);
   
   /**
     * Fill a fake buffer with cyclic bytes sequence 0 to 255.
     * Only for simulate bytes receive from an another peer (downloaded bytes).
     */
   static void fillBuffer(QByteArray& buffer);
   
   static const uint bufferSize; ///< The buffer size, some kilobytes
   
protected:
   /**
     * Start the downloading. Non-blocking call.
     */
   void run();
   
private :
   Chunk* chunk;   
};

#endif // DOWNLOADER_H
