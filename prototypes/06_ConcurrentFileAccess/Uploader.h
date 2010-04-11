#ifndef UPLOADER_H
#define UPLOADER_H

#include <QThread>
#include <QByteArray>

#include <Chunk.h>

class Uploader : public QThread
{
   Q_OBJECT
public:
   Uploader();

   void setChunk(Chunk* chunk);
    
protected:
   void run();
   
private:
   static QByteArray refArray; ///< The reference array, each read buffer should be equal to this array.
   Chunk* chunk; ///< The associated chunk, can be 0 if there is no chunk.
};

#endif
