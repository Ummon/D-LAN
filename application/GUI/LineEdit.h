#ifndef GUI_LINEEDIT_H
#define GUI_LINEEDIT_H

#include <QLineEdit>
#include <QKeyEvent>

namespace GUI
{
   class LineEdit : public QLineEdit
   {
      Q_OBJECT
   public:
      explicit LineEdit(QWidget* parent = 0);

   signals:
      void returnPressed(Qt::KeyboardModifiers);

   protected:
      void keyPressEvent(QKeyEvent* event);
   };
}

#endif
