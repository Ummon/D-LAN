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
