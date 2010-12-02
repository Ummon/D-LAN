#ifndef GUI_WIDGETDOWNLOADS_H
#define GUI_WIDGETDOWNLOADS_H

#include <QWidget>
#include <QPoint>
#include <QStyledItemDelegate>

#include <DownloadsModel.h>
#include <PeerListModel.h>
#include <CoreConnection.h>
#include <CheckBoxList.h>

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
   };

   class WidgetDownloads : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetDownloads(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent = 0);
      ~WidgetDownloads();

   private slots:
      void displayContextMenuDownloads(const QPoint& point);
      void removeSelectedEntries();
      void removeCompletedFiles();

   private:
      Ui::WidgetDownloads *ui;

      CheckBoxList* filterStatusList;

      CoreConnection& coreConnection;

      DownloadsModel downloadsModel;
      DownloadsDelegate downloadsDelegate;
   };
}

#endif
