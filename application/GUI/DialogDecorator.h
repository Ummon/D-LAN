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

#include <QObject>
#include <QDialog>

namespace GUI
{
   class DialogDecorator : public QObject
   {
   public:
      explicit DialogDecorator(QObject* parent = nullptr);

      void setFrameWidths(int fixedSizeFrameWidth, int resizableFrameWidth);

   protected:
      bool eventFilter(QObject* obj, QEvent* event) override;

   private:
      void decorate(QDialog* dialog);

      int fixedSizeFrameWidth;
      int resizableFrameWidth;
   };
}
