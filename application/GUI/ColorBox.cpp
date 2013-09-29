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
  
#include <ColorBox.h>
using namespace GUI;

#include <QPaintEngine>
#include <QPainter>
#include <QRect>
#include <QColorDialog>

/**
  * @class ColorBox
  * A class to display a choosen color into a button.
  */

ColorBox::ColorBox(QWidget* parent) :
   QPushButton(parent)
{
   connect(this, SIGNAL(clicked()), this, SLOT(chooseColor()));
}

void ColorBox::setColor(const QColor& color)
{
   if (this->currentColor != color)
   {
      this->currentColor = color;
      emit colorChanged(this->currentColor);
      this->repaint();
   }
}

QColor ColorBox::getCurrentColor() const
{
   return this->currentColor;
}

void ColorBox::chooseColor()
{
   QColor newColor = QColorDialog::getColor(this->currentColor, this);

   if (newColor != this->currentColor)
   {
      this->currentColor = newColor;
      emit colorChanged(this->currentColor);
   }
}

void ColorBox::paintEvent(QPaintEvent* event)
{
   QPushButton::paintEvent(event);

   QPainter painter(this);

   painter.setPen(Qt::NoPen);
   painter.setBrush(QBrush(this->currentColor));
   QSize size = this->size();
   size.setHeight(size.height() - 8);
   size.setWidth(size.width() - 8);
   painter.drawRect(QRect(QPoint(4, 4), size));
}
