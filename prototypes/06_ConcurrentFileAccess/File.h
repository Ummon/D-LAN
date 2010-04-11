#ifndef FILE_H
#define FILE_H

#include <exception>

#include <QList>
#include <QString>
#include <QFile>
#include <QMutex>

class FileSizeDoesntMatchException : public std::exception {};

class Chunk;

class File
{
public:
   static const uint chunkSize = 16777216; // 16 MB

   File(const QString& name, quint64 size) throw(FileSizeDoesntMatchException);
   
   virtual ~File();

   bool write(const QByteArray& buffer, qint64 offset);
   qint64 read(QByteArray& buffer, qint64 offset);
   const QList<Chunk*>& getChunks();
   
private:
   QFile* fileInWriteMode;
   QFile* fileInReadMode;

   quint64 size;

   QMutex writeLock;
   QMutex readLock;
   
   QList<Chunk*> chunks;
};

#endif
