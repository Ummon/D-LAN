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

#include <QString>
#include <QStyleOptionViewItem>

namespace GUI
{
   /**
     * In a right-to-left interface a text is laid out right-to-left whatever its content: a size as "1.5 GiB" is
     * then shown as "GiB 1.5" and a path as "Music/" as "/Music".
     * Returns the text embedded as left-to-right if it doesn't begin with a right-to-left character, as a label
     * already does by itself. Only the text direction is concerned, the alignment remains the one of the interface.
     */
   inline QString withDirectionFromContent(const QString& text, Qt::LayoutDirection direction)
   {
      static const QChar LEFT_TO_RIGHT_EMBEDDING(0x202A);
      static const QChar POP_DIRECTIONAL_FORMATTING(0x202C);

      if (direction != Qt::RightToLeft || text.isEmpty() || text.isRightToLeft())
         return text;
      return LEFT_TO_RIGHT_EMBEDDING + text + POP_DIRECTIONAL_FORMATTING;
   }

   /**
     * To be called by the 'initStyleOption(..)' of the item delegates.
     */
   inline void setTextDirectionFromContent(QStyleOptionViewItem* option)
   {
      option->text = withDirectionFromContent(option->text, option->direction);
   }
}
