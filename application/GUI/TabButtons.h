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
  
#ifndef GUI_TABBUTTONS_H
#define GUI_TABBUTTONS_H

#include <QAbstractButton>
#include <QStyleOption>
#include <QPainter>

namespace GUI
{
   class TabButton : public QAbstractButton
   {
      Q_OBJECT
   public:
      TabButton(QWidget* parent = 0);
      virtual QSize sizeHint() const;
      virtual QSize minimumSizeHint() const;
      virtual void enterEvent(QEvent *event);
      virtual void leaveEvent(QEvent *event);
      virtual void paintEvent(QPaintEvent *event);

   protected:
      virtual void drawPrimitive(const QStyleOption& opt, QPainter& p) = 0;
   };

/////

   class TabCloseButton : public TabButton
   {
       Q_OBJECT
   public:
       TabCloseButton(QWidget* widget, QWidget* parent = 0);

   protected:
       void drawPrimitive(const QStyleOption& opt, QPainter& p);

   signals:
      void clicked(QWidget* widget);

   private slots:
      void buttonClicked();

   private:
       QWidget* widget;
   };

/////

   class TabRefreshButton : public TabButton
   {
       Q_OBJECT
   public:
       TabRefreshButton(QWidget* parent = 0);

   private:
       void drawPrimitive(const QStyleOption& option, QPainter& painter);

       QIcon icon;
   };
}

#endif
