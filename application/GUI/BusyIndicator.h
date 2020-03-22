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

#include <QWidget>
#include <QElapsedTimer>
#include <QTimer>
#include <QColor>
#include <QMetaType>

namespace GUI
{
   class BusyIndicator : public QWidget
   {
      Q_OBJECT
   public:
      BusyIndicator(QWidget* parent = 0, int refreshRate = 30, int animationDuration = 1200, int nbOfPoints = 12, int radiusOfThePoints = 20);

      QSize	sizeHint() const;

   protected:
      void paintEvent(QPaintEvent* event);
      void hideEvent(QHideEvent* event);
      void showEvent(QShowEvent* event);

   private:
      const int ANIMATION_DURATION; // Time to do a complete rotation.
      const int NB_OF_POINTS;
      const int RADIUS_OF_THE_POINTS;

      QTimer refreshTimer;
      QElapsedTimer timer;
   };
}
