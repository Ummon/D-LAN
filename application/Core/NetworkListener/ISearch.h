#ifndef NETWORKLISTENER_ISEARCH_H
#define NETWORKLISTENER_ISEARCH_H

#include <QObject>
#include <QString>

#include <Protos/common.pb.h>

namespace NL
{
   class ISearch : public QObject
   {
      Q_OBJECT
   public:
      virtual ~ISearch() {}

      /**
        * Begin a new search. This function can be called only ONE time.
        * @return An associated tag. This tag will be repeated in the result, see the signal 'found'.
        */
      virtual quint64 search(const QString& words) = 0;

      /**
        * @return ms elapsed from the call to 'search'.
        */
      virtual qint64 elapsed() = 0;

   signals:
      void found(const Protos::Common::FindResult& result);
   };
}
#endif
