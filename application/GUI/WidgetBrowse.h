#ifndef GUI_WIDGETBROWSE_H
#define GUI_WIDGETBROWSE_H

#include <QWidget>

#include <Common/Hash.h>

#include <PeerListModel.h>
#include <CoreConnection.h>
#include <BrowseModel.h>

namespace Ui {
   class WidgetBrowse;
}

namespace GUI
{
   class WidgetBrowse : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetBrowse(CoreConnection& coreConnection, PeerListModel& model, const Common::Hash& peerID, QWidget *parent = 0);
      ~WidgetBrowse();
      Common::Hash getPeerID() const;

   private:
      Ui::WidgetBrowse *ui;

      CoreConnection& coreConnection;
      PeerListModel& peerListModel;
      Common::Hash peerID;

      BrowseModel browseModel;
   };
}
#endif
