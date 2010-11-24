#ifndef GUI_WIDGETBROWSE_H
#define GUI_WIDGETBROWSE_H

#include <QWidget>
#include <QStyledItemDelegate>

#include <Common/Hash.h>

#include <PeerListModel.h>
#include <CoreConnection.h>
#include <BrowseModel.h>

namespace Ui {
   class WidgetBrowse;
}

namespace GUI
{
   class BrowseDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class WidgetBrowse : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetBrowse(CoreConnection& coreConnection, PeerListModel& peerListModel, const Common::Hash& peerID, QWidget *parent = 0);
      ~WidgetBrowse();
      Common::Hash getPeerID() const;

   private slots:
      void displayContextMenuPeers(const QPoint& point);
      void download();

   private:
      Ui::WidgetBrowse* ui;

      CoreConnection& coreConnection;
      PeerListModel& peerListModel;
      const Common::Hash peerID;

      BrowseModel browseModel;
      BrowseDelegate browseDelegate;
   };
}
#endif
