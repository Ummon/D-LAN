#ifndef GUI_WIDGETDOWNLOADS_H
#define GUI_WIDGETDOWNLOADS_H

#include <QWidget>
#include <QStyledItemDelegate>

#include <DownloadsModel.h>
#include <PeerListModel.h>
#include <CoreConnection.h>

namespace Ui {
   class WidgetDownloads;
}

namespace GUI
{
   class DownloadsDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
      void drawFocus(QPainter*, const QStyleOptionViewItem&, const QRect&) const {}
   };

   class WidgetDownloads : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetDownloads(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent = 0);
      ~WidgetDownloads();

   private:
      Ui::WidgetDownloads *ui;

      DownloadsModel downloadsModel;
      DownloadsDelegate downloadsDelegate;
   };
}

#endif
