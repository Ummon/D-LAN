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
  
#include <Log/LogDelegate.h>
using namespace GUI;

#include <QPainter>
#include <QModelIndex>

#include <Log/LogModel.h>

void LogDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const LogModel* model = static_cast<const LogModel*>(index.model());

   QStyleOptionViewItem newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   switch (model->getSeverity(index.row()))
   {
   case LM::SV_WARNING:
      painter->fillRect(option.rect, QColor(235, 199, 199));
      break;
   case LM::SV_ERROR:
      painter->fillRect(option.rect, QColor(200, 0, 0));
      newOption.palette.setColor(QPalette::Text, QColor(255, 255, 255));
      break;
   case LM::SV_FATAL_ERROR:
      painter->fillRect(option.rect, QColor(50, 0, 0));
      newOption.palette.setColor(QPalette::Text, QColor(255, 255, 0));
      break;
   default:;
   }

   QStyledItemDelegate::paint(painter, newOption, index);
}
