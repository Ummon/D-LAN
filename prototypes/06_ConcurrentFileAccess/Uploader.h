#ifndef UPLOADER_H
#define UPLOADER_H

#include <QThread>
#include <QByteArray>

#include <Chunk.h>

/**
  * Will read a chunk. The data are send to a peer (not implemented).
  */
class Uploader : public QThread
{
public:
   Uploader();
      
   /**
     * Defines a chunk, must be called prior 'run()'.
     * @param chunk The chunk.
     */
   void setChunk(Chunk* chunk);
    
protected:
   /**
     * Start the downloading. Non-blocking call.
     */
   void run();
   
private:
   static QByteArray refArray; ///< The reference array, each read buffer should be equal to this array.
   
   Chunk* chunk;
};

#endif // UPLOADER_H
