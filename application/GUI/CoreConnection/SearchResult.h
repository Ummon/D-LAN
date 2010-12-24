#ifndef GUI_SEARCHRESULT_H
#define GUI_SEARCHRESULT_H

#include <QString>

#include <Protos/common.pb.h>

#include <CoreConnection/ISearchResult.h>

namespace GUI
{
   class CoreConnection;

   class SearchResult : public ISearchResult
   {
      Q_OBJECT
   public:
      SearchResult(CoreConnection* coreConnection, const QString& terms);
      void start();
      void setTag(quint64 tag);

   private slots:
      void searchResult(const Protos::Common::FindResult& findResult);

   private:
      CoreConnection* coreConnection;
      const QString terms;
      quint64 tag;
   };
}

#endif
