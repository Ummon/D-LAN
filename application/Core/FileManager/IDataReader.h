#ifndef FILEMANAGER_IDATAREADER_H
#define FILEMANAGER_IDATAREADER_H

#include <QObject>
#include <QByteArray>

namespace FM
{
   class IDataReader : virtual public QObject
   {
      Q_OBJECT
   public:

      virtual ~IDataReader() {}

      /**
        * Non-blockant method.
        */
      virtual void read(uint offset) = 0;

   signals:
      /**
        * Emitted when the read is terminated.
        * If an error occurs, data is null.
        * Data is valid until an other read is called or the IDataReader object is deleted.
        */
      void readFinished(char* data, int nbBytesRead);
   };
}

#endif
