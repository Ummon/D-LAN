#include <QtCore/QCoreApplication>
#include <QtCore/QCryptographicHash>
#include <QtCore/QFile>
#include <QtCore/QDebug>
#include <QtCore/QList>

static const int BUFFER_SIZE = 65536; // (64kB) Buffer used when reading a file.
static const int CHUNK_SIZE = 2097152; // (2 MB)

class FileNotFoundException : public std::exception {};

/**
  * Compute the SHA1 hash for the given file.
  * @param filename The filename (with or without prefixed by a path). If the file
  * is not found the exeption 'FileNotFoundException' is thrown.
  * @param bufferSize The size of the buffer used to give the data to 'QCryptographicHash'.
  * @return The sha1 hash.
  */
QByteArray computeSHA1(const QString& filename, qint32 bufferSize)
      throw (FileNotFoundException)
{
   QCryptographicHash crypto(QCryptographicHash::Sha1);
      
   QFile file(filename);
   if (!file.open(QIODevice::ReadOnly))
      throw FileNotFoundException();

   char buffer[bufferSize];
   qint64 bytesRead = 0;
   while ((bytesRead = file.read(buffer, bufferSize)) > 0)
      crypto.addData(buffer, bytesRead);
      
   return crypto.result();
}

/**
  * Compute some SHA1 hash for each chunk of the given file.
  * @param filename The filename (with or without prefixed by a path). If the file
  * is not found the exeption 'FileNotFoundException' is thrown.
  * @param chunkSize The size of each chunk. It must be a divisor of 'bufferSizer'.
  * @param bufferSize The size of the buffer used to give the data to 'QCryptographicHash'.
  * @return A list of chunk hashes.
  */
QList<QByteArray> computeMultiSHA1(const QString& filename, qint32 chunkSize, qint32 bufferSize)
      throw (FileNotFoundException)
{
   QList<QByteArray> result;
   
   QCryptographicHash crypto(QCryptographicHash::Sha1);
      
   QFile file(filename);
   if (!file.open(QIODevice::ReadOnly))
      throw FileNotFoundException();

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

/**
  * Entry point.
  * @param argc Must be equal to 3.
  * @param argv Must contain :
  *  1) the executable name.
  *  2) "all" to compute the sha1 of the whole file or "chunk" for compute sha1 of each chunk.
  *  3) The filename.
  * @return 0 if all done fine else > 0
  */
int main(int argc, char* argv[])
{
   QTextStream out(stdout);

   if (argc != 3)
   {
      out << "Usage : " << argv[0] << " (all|chunk) <file>" << endl;
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
