#ifndef GUI_PEERLISTDELEGATE_H
#define GUI_PEERLISTDELEGATE_H

#include <QStyledItemDelegate>
#include <QPixmap>

namespace GUI
{
   class PeerListDelegate : public QStyledItemDelegate
   {
      Q_OBJECT
      static const QColor DOWNLOAD_COLOR;
      static const QColor UPLOAD_COLOR;

   public:
      PeerListDelegate() : miniChartBackground(0) {}
      ~PeerListDelegate() { if (this->miniChartBackground) delete this->miniChartBackground; }

      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;

   private:
      const QPixmap& getMiniChartBackground(int width, int height) const;

      mutable QPixmap* miniChartBackground; // Caching to improve performance.
   };
}

#endif
