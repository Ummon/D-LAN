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

#pragma once

#include <QStyledItemDelegate>

namespace GUI
{
   /**
     * An item delegate which doesn't draw the focus rectangle around the current item.
     * It can also show the selection only while the view belongs to the active window.
     */
   class NoFocusDelegate : public QStyledItemDelegate
   {
   public:
      explicit NoFocusDelegate(bool selectionOnlyIfActive = false, QObject* parent = nullptr) :
         QStyledItemDelegate(parent), selectionOnlyIfActive(selectionOnlyIfActive)
      {}

   protected:
      void initStyleOption(QStyleOptionViewItem* option, const QModelIndex& index) const override
      {
         QStyledItemDelegate::initStyleOption(option, index);

         option->state &= ~QStyle::State_HasFocus;

         if (this->selectionOnlyIfActive && !(option->state & QStyle::State_Active))
            option->state &= ~QStyle::State_Selected;
      }

   private:
      const bool selectionOnlyIfActive;
   };
}
