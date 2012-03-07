#include <LineEdit.h>
using namespace GUI;

LineEdit::LineEdit(QWidget* parent) :
   QLineEdit(parent)
{
}

void LineEdit::keyPressEvent(QKeyEvent* event)
{
   if (event->key() == Qt::Key_Return)
   {
      emit returnPressed(event->modifiers());
   }

   QLineEdit::keyPressEvent(event);
}

