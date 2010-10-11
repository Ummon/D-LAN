#ifndef FILEMANAGER_IDATAWRITER_H
#define FILEMANAGER_IDATAWRITER_H

#include <QObject>

#include <QByteArray>

namespace FM
{
   class IDataWriter : virtual public QObject
   {
      Q_OBJECT
   public:
      enum Status
      {
         NOT_FINISHED,
         END_OF_CHUNK,
         IO_ERROR
      };

      virtual ~IDataWriter() {}

      virtual char* getBuffer() = 0;
      virtual int getBufferSize() const = 0;

      /**
        * Non-blockant method.
        */
      virtual void write(int nbBytes) = 0;

   signals:
      void writeFinished(FM::IDataWriter::Status status);
   };
}

#endif
