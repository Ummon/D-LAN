/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <TableLogItemDelegate.h>

#include <QPainter>

#include <Common/LogManager/IEntry.h>

#include <TableLogModel.h>

/**
  * @class TableLogItemDelegate
  *
  * Override the paint method for table log items and
  * draw them in fonction of their severity.
  */

TableLogItemDelegate::TableLogItemDelegate(QObject *parent) :
    QItemDelegate(parent)
{
}

void TableLogItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const TableLogModel* model = static_cast<const TableLogModel*>(index.model());
   QStyleOptionViewItemV4 newOption(option);

   switch (model->getSeverity(index.row()))
   {
   case LM::SV_END_USER :
      painter->fillRect(option.rect, QColor(222, 213, 235));
      break;
   case LM::SV_WARNING :
      painter->fillRect(option.rect, QColor(235, 199, 199));
      break;
   case LM::SV_ERROR :
      painter->fillRect(option.rect, QColor(200, 0, 0));
      newOption.palette.setColor(QPalette::Text, QColor(255, 255, 255));
      break;
   case LM::SV_FATAL_ERROR :
      painter->fillRect(option.rect, QColor(50, 0, 0));
      newOption.palette.setColor(QPalette::Text, QColor(255, 255, 0));
      break;
   // No special color for these cases.
   case LM::SV_DEBUG :
   case LM::SV_UNKNOWN :
   default:;
   }

   QItemDelegate::paint(painter, newOption, index);
}

/*QSize TableLogItemDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index ) const
{
   QSize s = QItemDelegate::sizeHint(option, index);
   if (s.isValid())
   {
      s.setHeight(0.5 * s.height());
   }
   return s;
}*/
