#include <QtCore/QCoreApplication>
#include <QtCore/QCryptographicHash>
#include <QtCore/QFile>
#include <QtCore/QDebug>
#include <QtCore/QList>

#define BUFFER_SIZE 65536 // Buffer used when reading a file.
#define CHUNK_SIZE 2097152 // 2 MB

/**
  * Compute the SHA1 hash for the given file.
  */
QByteArray computeSHA1(const QString& filename, qint32 bufferSize)
{
   QCryptographicHash crypto(QCryptographicHash::Sha1);
      
   QFile file(filename);
   if (!file.open(QIODevice::ReadOnly))
      return QByteArray();

   char buffer[bufferSize];
   qint64 bytesRead = 0;
   while ((bytesRead = file.read(buffer, bufferSize)) > 0)
   {
      crypto.addData(buffer, bytesRead);
   }
      
   return crypto.result();
}

/**
  * Compute some SHA1 hash for each chunk of the given file.
  * 'chunkSize' must be a divisor of 'bufferSizer'.
  */
QList<QByteArray> computeMultiSHA1(const QString& filename, qint32 chunkSize, qint32 bufferSize)
{
   QList<QByteArray> result;
   
   QCryptographicHash crypto(QCryptographicHash::Sha1);
      
   QFile file(filename);
   if (!file.open(QIODevice::ReadOnly))
      return QList<QByteArray>();

   char buffer[bufferSize];
   bool endOfFile = false;
   while (!endOfFile)
   {
      qint64 bytesReadTotal = 0;
      while (bytesReadTotal < chunkSize)
      {
         qint64 bytesRead = file.read(buffer, bufferSize);
         if (bytesRead == 0)
         {
            endOfFile = true;
            break;
         }
         crypto.addData(buffer, bytesRead);
         bytesReadTotal += bytesRead;
      }
      result.append(crypto.result());
      crypto.reset();
   }
      
   return result;
}

int main(int argc, char *argv[])
{
   QTextStream out(stdout);

   if (argc != 3)
   {
      out << "Usage : " << argv[0] << " (all|chunk) <file>";
      return 1;
   }
   
   QString mode = QString(argv[1]);   
   if (mode == "all")
   {
      out << computeSHA1(argv[2], BUFFER_SIZE).toHex() << "\n";
   }
   else if (mode == "chunk")
   {
      QList<QByteArray> shas = computeMultiSHA1(argv[2], CHUNK_SIZE, BUFFER_SIZE);
      foreach (QByteArray sha, shas)
         out << sha.toHex() << "\n";
   }
      
   return 0;
}
