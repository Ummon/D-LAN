#include <QtCore/QCoreApplication>

#include <QDebug>

#include <Downloader.h>
#include <Uploader.h>
#include <File.h>
#include <QTextCodec>

const quint64 filesize = 104857600;  // 100 MB
const QString filename("test.bin");

int main(int argc, char *argv[])
{   
   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));
   
   QCoreApplication a(argc, argv);  
    
   File f(filename, filesize);
   qDebug() << "File " << filename << " size = " << hex << (quint32)filesize;
        
   const QList<Chunk*> chunks = f.getChunks();
   QList<Downloader*> downloaders;
   QList<Uploader*> uploaders;
    
   int i = 0;
   foreach (Chunk* chunk, chunks)
   {
      Downloader* d = new Downloader();
      downloaders.append(d);
      d->setChunk(chunk);
      d->start();
      
      Uploader* u = new Uploader();
      uploaders.append(u);
      u->setChunk(chunk);
      u->start();
      
      i += 1;
   }
    
   foreach (Downloader* d, downloaders)
      d->wait();
   
   foreach (Uploader* u, uploaders)
      u->wait();
      
   qDebug() << "All thread has terminated";
    
   // return a.exec();
}
