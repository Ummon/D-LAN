#ifndef TABLELOGITEMDELEGATE_H
#define TABLELOGITEMDELEGATE_H

#include <QItemDelegate>

class TableLogItemDelegate : public QItemDelegate
{
public:
    TableLogItemDelegate(QObject *parent = 0);

    void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
    //QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index ) const;
};

#endif
