#ifndef DOWNLOADMANAGER_DOWNLOADPREDICATE_H
#define DOWNLOADMANAGER_DOWNLOADPREDICATE_H

#include <QSet>
#include <QList>

namespace DM
{
   class Download;

   struct DownloadPredicate
   {
      virtual bool operator() (Download* download) = 0;
   };

   struct IsComplete : public DownloadPredicate
   {
      bool operator() (Download* download);
   };

   struct IsContainedInAList : public DownloadPredicate
   {
      IsContainedInAList(QList<quint64> downloadIDs);
      bool operator() (Download* download);

   private:
      QSet<quint64> downloadIDs;
   };
}

#endif
