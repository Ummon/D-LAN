#ifndef UPLOADER_H
#define UPLOADER_H

#include <QThread>
#include <QByteArray>

#include <Chunk.h>

/**
  * Read a chunk. The data are send to a peer (not implemented).
  */
class Uploader : public QThread
{
   Q_OBJECT
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
   Chunk* chunk; ///< The associated chunk, can be 0 if there is no chunk.
};

#endif // UPLOADER_H
