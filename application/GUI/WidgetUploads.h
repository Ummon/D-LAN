#ifndef GUI_WIDGETUPLOADS_H
#define GUI_WIDGETUPLOADS_H

#include <QWidget>
#include <QStyledItemDelegate>

#include <UploadsModel.h>
#include <PeerListModel.h>
#include <CoreConnection.h>

namespace Ui {
   class WidgetUploads;
}

namespace GUI
{
   class UploadsDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
      void drawFocus(QPainter*, const QStyleOptionViewItem&, const QRect&) const {}
   };

   class WidgetUploads : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetUploads(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent = 0);
      ~WidgetUploads();

   private:
      Ui::WidgetUploads *ui;

      UploadsModel uploadsModel;
      UploadsDelegate uploadsDelegate;
   };
}

#endif
