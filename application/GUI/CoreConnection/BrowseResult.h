#ifndef GUI_BROWSERESULT_H
#define GUI_BROWSERESULT_H

#include <QtCore>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <CoreConnection/IBrowseResult.h>

namespace GUI
{
   class CoreConnection;

   class BrowseResult : public IBrowseResult
   {
      Q_OBJECT
   public:
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID);
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entry& entry);
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots = true);
      void start();
      void setTag(quint64 tag);

   private slots:
      void browseResult(const Protos::GUI::BrowseResult& browseResult);

   private:
      void init(CoreConnection* coreConnection);

      CoreConnection* coreConnection;
      const Common::Hash peerID;
      Protos::GUI::Browse browseMessage;
      quint64 tag;
   };
}

#endif
