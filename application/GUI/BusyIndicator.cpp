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
  
#include <BusyIndicator.h>
using namespace GUI;

#include <QPainter>
#include <QStyleOption>

/**
  * @class GUI::BusyIndicator
  *
  * A widget to show there is a task currently running.
  * It uses the 'QPalette::Hightlight' color to draw the widget.
  */

/**
  * @param refreshRate [ms] default is 30 ms which is equal to 33 Hz.
  * @param animationDuration time to complete a round [ms]
  * @param radiusOfThePoints must be set between 1 and 50.
  */
BusyIndicator::BusyIndicator(QWidget* parent, int refreshRate, int animationDuration, int nbOfPoints, int radiusOfThePoints) :
   QWidget(parent),
   ANIMATION_DURATION(animationDuration),
   NB_OF_POINTS(nbOfPoints),
   RADIUS_OF_THE_POINTS(radiusOfThePoints > 50 ? 50 : radiusOfThePoints)
{
   this->timer.start();

   this->refreshTimer.setInterval(refreshRate);
   connect(&this->refreshTimer, SIGNAL(timeout()), this, SLOT(update()));
   this->refreshTimer.start();
}

QSize	BusyIndicator::sizeHint() const
{
   return QSize(16, 16);
}

void BusyIndicator::paintEvent(QPaintEvent* /*event*/)
{
   const int side = qMin(this->width(), this->height());

   QPainter painter(this);
   painter.setRenderHint(QPainter::Antialiasing);
   painter.translate(this->width() / 2, this->height() / 2);
   painter.scale(side / 200.0, side / 200.0);

   painter.setPen(Qt::NoPen);

   QStyleOption opt;
   opt.init(this);
   QColor currentColor = opt.palette.brush(QPalette::Highlight).color();

   for (int i = 0; i < NB_OF_POINTS; i++)
   {
      currentColor.setAlphaF(qreal(i) / NB_OF_POINTS);
      painter.setBrush(currentColor);

      painter.save();
      painter.rotate(i * 360.0 / NB_OF_POINTS + 360.0 * qreal(this->timer.elapsed() % ANIMATION_DURATION) / ANIMATION_DURATION);
      painter.drawEllipse(QPoint(100 - RADIUS_OF_THE_POINTS, 0), RADIUS_OF_THE_POINTS, RADIUS_OF_THE_POINTS);
      painter.restore();
   }
}

void BusyIndicator::hideEvent(QHideEvent* /*event*/)
{
   this->refreshTimer.stop();
}

void BusyIndicator::showEvent(QShowEvent* /*event*/)
{
   this->refreshTimer.start();
}
