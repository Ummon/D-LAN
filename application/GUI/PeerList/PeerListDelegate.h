#ifndef GUI_PEERLISTDELEGATE_H
#define GUI_PEERLISTDELEGATE_H

#include <QStyledItemDelegate>

namespace GUI
{
   class PeerListDelegate : public QStyledItemDelegate
   {
      Q_OBJECT
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };
}

#endif
