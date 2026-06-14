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
#include <QPalette>
#include <QGuiApplication>
#include <QModelIndex>

#include <Log/LogModel.h>

const QColor LogDelegate::COLOR_WARNING(150, 20, 20);

const QColor LogDelegate::COLOR_BACKGROUND_ERROR(200, 0, 0);
const QColor LogDelegate::COLOR_FOREGROUND_ERROR(255, 255, 255);

const QColor LogDelegate::COLOR_BACKGROUND_FATAL_ERROR(50, 0, 0);
const QColor LogDelegate::COLOR_FOREGROUND_FATAL_ERROR(255, 255, 0);

void LogDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const LogModel* model = static_cast<const LogModel*>(index.model());

   QStyleOptionViewItem newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   switch (model->getSeverity(index.row()))
   {
   case LM::SV_WARNING:
      {
         auto color = QGuiApplication::palette().color(QPalette::Normal, QPalette::Window).toHsl();
         color.setHslF(COLOR_WARNING.hslHueF(), COLOR_WARNING.hslSaturationF(), color.lightnessF());
         painter->fillRect(option.rect, color);
      }
      break;
   case LM::SV_ERROR:
      painter->fillRect(option.rect, COLOR_BACKGROUND_ERROR);
      newOption.palette.setColor(QPalette::Text, COLOR_FOREGROUND_ERROR);
      break;
   case LM::SV_FATAL_ERROR:
      painter->fillRect(option.rect, COLOR_BACKGROUND_FATAL_ERROR);
      newOption.palette.setColor(QPalette::Text, COLOR_FOREGROUND_FATAL_ERROR);
      break;
   default:;
   }

   QStyledItemDelegate::paint(painter, newOption, index);
}
