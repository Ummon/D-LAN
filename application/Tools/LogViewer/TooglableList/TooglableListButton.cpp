#include "TooglableListButton.h"

#include <QMouseEvent>
TooglableListButton::TooglableListButton(QWidget* parent) :
   QPushButton(parent)
{
}

bool TooglableListButton::event(QEvent* event)
{
   if (event->type() == QEvent::MouseButtonRelease)
   {
      QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
      if (mouseEvent->button() == Qt::RightButton)
         emit rightClicked();
   }

   return QPushButton::event(event);
}
