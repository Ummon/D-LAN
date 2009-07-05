#ifndef FILE_H
#define FILE_H

#include <exception>

#include <QList>
#include <QString>
#include <QFile>
#include <QReadWriteLock>

class FileSizeDoesntMatch : public std::exception {};

class Chunk;

/**
  * A file can be filled by a downloader or read by an uploader.
  * It has a fixed size.
  * 'read' and 'write' are thread safe and a 'QReadWriteLock'.
  */
class File
{
public:
   static const uint chunkSize = 16777216; // 16 MB
   
   /**
     * Create a new empty file or open an existing one.
     * If the file already exists the given size must match.
     */
   File(const QString& name, quint64 size) throw(FileSizeDoesntMatch);
   
   virtual ~File();
   
   /**
     * Write some bytes to the file at the given offset.
     * If the buffer exceed the file size then only the begining of the buffer is
     * used, the file is not resizing.
     * @param buffer The buffer.
     * @param offset An offset.
     * @return true if end of file reached.
     */
   bool write(const QByteArray& buffer, qint64 offset);
   
   /**
     * Fill the buffer with the read bytes from the given offset.
     * If the end of file is reached the buffer will be partialy filled.
     * @param buffer The buffer.
     * @param offset An offset.
     * @return the number of bytes read.
     */
   qint64 read(QByteArray& buffer, qint64 offset);
   
   /**
     * @return A collection of the file's chunks.
     */
   const QList<Chunk*>& getChunks();
   
private:
   QFile* file;
   quint64 size;
   QReadWriteLock lock;
   
   QList<Chunk*> chunks;
};

#endif // FILE_H
