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
