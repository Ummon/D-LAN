#include <Settings/SharedEntryListDelegate.h>
using namespace GUI;

#include <Settings/SharedEntryListModel.h>

QWidget* SharedEntryListDelegate::createEditor(
   QWidget *parent,
   const QStyleOptionViewItem &opt,
   const QModelIndex &index
) const
{
   emit editingStarted(index);
   return QStyledItemDelegate::createEditor(parent, opt, index);
}

void SharedEntryListDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItem newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

QSize SharedEntryListDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   auto size = QStyledItemDelegate::sizeHint(option, index);

   // An hack to avoid truncating the size field, don't know why it happens (Qt add "..." at the end of some sizes).
   if (index.column() == SharedEntryListModel::SIZE)
      size.rwidth() += 2 * option.fontMetrics.averageCharWidth();

   return size;
}
