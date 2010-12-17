#include <QtCore/QCoreApplication>
#include <QtCore/QTextStream>
#include <QtCore/QFile>
#include <QtCore/QDebug>
#include <QtCore/QList>

extern "C" {
   #include "blake_opt.h"
}

// In bits, can be 224, 256, 385, 512
#define HASH_SIZE 224

static const int BUFFER_SIZE_512 = 524288; // (512kB)
static const int BUFFER_SIZE_64 = 65536; // (64kB)
static const int BUFFER_SIZE_4 = 4096; // (4kB)

static const int BUFFER_SIZE = BUFFER_SIZE_64; // Buffer used when reading a file.

class FileNotFoundException : public std::exception {};

/**
  * Compute the hash for the given file.
  * @param filename The filename (with or without prefixed by a path). If the file
  * is not found the exeption 'FileNotFoundException' is thrown.
  * @param bufferSize The size of the buffer
  * @return The hash.
  */
QByteArray computeBlake(const QString& filename, qint32 bufferSize)
      throw (FileNotFoundException)
{
   QFile file(filename);
   if (!file.open(QIODevice::ReadOnly))
      throw FileNotFoundException();

   hashState S;
   unsigned char bufferHash[HASH_SIZE / 8];
   Init(&S, HASH_SIZE);

   char buffer[bufferSize];
   qint64 bytesRead = 0;
   while ((bytesRead = file.read(buffer, bufferSize)) > 0)
   {
      Update(&S, (BitSequence*)buffer, bytesRead * 8);
   }

   Final(&S, bufferHash);
   return QByteArray((const char*)bufferHash, HASH_SIZE / 8);
}

/**
  * Entry point.
  * @param argc Must be equal to 3.
  * @param argv Must contain :
  *  1) the executable name.
  *  2) The filename.
  * @return 0 if all done fine else > 0
  */
int main(int argc, char* argv[])
{
   QTextStream out(stdout);

   if (argc != 2)
   {
      out << "Usage : " << argv[0] << " <file>" << endl;
      return 1;
   }

   out << computeBlake(argv[1], BUFFER_SIZE).toHex() << "\n";

   return 0;
}
