#include <priv/DownloadPredicate.h>
using namespace DM;

#include <IDownload.h>
#include <priv/Download.h>

bool IsComplete::operator() (Download* download)
{
   return download->getStatus() == COMPLETE;
}

/**
  * We us a QSet to decrease the complexity.
  */
IsContainedInAList::IsContainedInAList(QList<quint64> downloadIDs) :
   downloadIDs(downloadIDs.toSet())
{
}

bool IsContainedInAList::operator() (Download* download)
{
   return this->downloadIDs.contains(download->getID());
}
