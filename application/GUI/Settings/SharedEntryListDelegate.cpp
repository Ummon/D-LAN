#include <Settings/SharedEntryListDelegate.h>
using namespace GUI;

QWidget* SharedEntryListDelegate::createEditor(
   QWidget *parent,
   const QStyleOptionViewItem &opt,
   const QModelIndex &index
) const
{
   emit editingStarted(index);
   return QStyledItemDelegate::createEditor(parent, opt, index);
}