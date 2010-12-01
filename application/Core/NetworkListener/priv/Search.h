#ifndef NETWORKLISTENER_SEARCH_H
#define NETWORKLISTENER_SEARCH_H

#include <QSharedPointer>
#include <QElapsedTimer>

#include <Libs/MersenneTwister.h>

#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>

#include <ISearch.h>
#include <priv/UDPListener.h>

namespace NL
{
   class Search : public ISearch, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Search(UDPListener& uDPListener);
      quint64 search(const QString& words);
      qint64 elapsed();

   private slots:
      void newFindResult(const Protos::Common::FindResult& result);

   private:
      UDPListener& uDPListener;

      bool searchLaunched;
      QDateTime dateOfLaunch;

      quint64 tag;
      MTRand mtrand;

      QElapsedTimer timer;
   };
}
#endif
