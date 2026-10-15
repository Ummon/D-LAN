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

#include <WindowFrame.h>
using namespace GUI;

#include <QApplication>
#include <QMouseEvent>
#include <QWindow>

/**
  * @class GUI::WindowFrame
  *
  * Lets the user resize a frameless window by dragging the frame drawn around it by a custom style. The width of the
  * frame is the left contents margin of the window, no margin means no frame. The window must track the mouse to show
  * the resize cursor over the frame. A window with a fixed size can't be resized.
  */

WindowFrame::WindowFrame(QWidget* window) :
   QObject(window), window(window), cursorSet(false)
{
   window->installEventFilter(this);
}

bool WindowFrame::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->window)
   {
      switch (event->type())
      {
      case QEvent::MouseMove:
         if (static_cast<QMouseEvent*>(event)->buttons() == Qt::NoButton)
            this->updateCursor(static_cast<QMouseEvent*>(event)->position().toPoint());
         break;

      case QEvent::MouseButtonPress:
         {
            const QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
            const Qt::Edges edges = this->edgesAt(mouseEvent->position().toPoint());
            // The window manager does the resize.
            if (edges && mouseEvent->button() == Qt::LeftButton && this->window->windowHandle() && this->window->windowHandle()->startSystemResize(edges))
               return true;
         }
         break;

      case QEvent::Leave:
         this->updateCursor(QPoint(-1, -1));
         break;

      default:;
      }
   }
   // The cursor of a widget is also the one of its children: it's unset when the mouse goes from the frame to a child
   // (this filter is installed on the application while the cursor is set).
   else if (event->type() == QEvent::Enter)
      this->updateCursor(QPoint(-1, -1));

   return false;
}

/**
  * @return The edges of the window to move to resize it from the given position, none if the position isn't on the frame.
  */
Qt::Edges WindowFrame::edgesAt(const QPoint& pos) const
{
   const int frameWidth = this->window->contentsMargins().left();
   const QRect rect = this->window->rect();
   if (frameWidth <= 0 || this->window->minimumSize() == this->window->maximumSize() || !rect.contains(pos) ||
       rect.adjusted(frameWidth, frameWidth, -frameWidth, -frameWidth).contains(pos))
      return {};

   Qt::Edges edges;
   if (pos.x() < RESIZE_CORNER_SIZE)
      edges |= Qt::LeftEdge;
   else if (pos.x() >= rect.width() - RESIZE_CORNER_SIZE)
      edges |= Qt::RightEdge;
   if (pos.y() < RESIZE_CORNER_SIZE)
      edges |= Qt::TopEdge;
   else if (pos.y() >= rect.height() - RESIZE_CORNER_SIZE)
      edges |= Qt::BottomEdge;
   return edges;
}

void WindowFrame::updateCursor(const QPoint& pos)
{
   const Qt::Edges edges = this->edgesAt(pos);
   if (edges)
   {
      if (edges == (Qt::TopEdge | Qt::LeftEdge) || edges == (Qt::BottomEdge | Qt::RightEdge))
         this->window->setCursor(Qt::SizeFDiagCursor);
      else if (edges == (Qt::TopEdge | Qt::RightEdge) || edges == (Qt::BottomEdge | Qt::LeftEdge))
         this->window->setCursor(Qt::SizeBDiagCursor);
      else if (edges & (Qt::LeftEdge | Qt::RightEdge))
         this->window->setCursor(Qt::SizeHorCursor);
      else
         this->window->setCursor(Qt::SizeVerCursor);

      if (!this->cursorSet)
      {
         this->cursorSet = true;
         qApp->installEventFilter(this);
      }
   }
   // Only unset our own cursor, 'QMainWindow' sets one over the dock separators.
   else if (this->cursorSet)
   {
      this->window->unsetCursor();
      this->cursorSet = false;
      qApp->removeEventFilter(this);
   }
}
