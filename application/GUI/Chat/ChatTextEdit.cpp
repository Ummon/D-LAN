#include <GUI/Chat/ChatTextEdit.h>

#include <QKeyEvent>

using namespace GUI;

ChatTextEdit::ChatTextEdit(QWidget* parent) :
   QTextEdit(parent)
{
   // Ignore CTRL-I, we use it as a italic key combination.
   this->addIgnoreKeyCombination(KeyCombination { Qt::ControlModifier, 'i' } );
   this->addIgnoreKeyCombination(KeyCombination { Qt::ControlModifier, 'I' } );
}

void ChatTextEdit::addIgnoreKeyCombination(KeyCombination keyCombination)
{
   this->keyCombinationIgnored << keyCombination;
}

bool ChatTextEdit::event(QEvent* e)
{
   if (e->type() == QEvent::KeyPress || e->type() == QEvent::KeyRelease)
   {
      QKeyEvent* keyEvent = static_cast<QKeyEvent*>(e);

      for (QListIterator<KeyCombination> i(this->keyCombinationIgnored); i.hasNext();)
      {
         const KeyCombination& key = i.next();
         if (keyEvent->modifiers().testFlag(key.modifier) && keyEvent->key() == key.key)
         {
            keyEvent->ignore();
            return false;
         }
      }
   }

   return QTextEdit::event(e);
}
