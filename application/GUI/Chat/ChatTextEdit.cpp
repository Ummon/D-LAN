#include <GUI/Chat/ChatTextEdit.h>

#include <QKeyEvent>

using namespace GUI;

ChatTextEdit::ChatTextEdit(QWidget* parent) :
   QTextEdit(parent)
{
   // Ignore CTRL-I, we use it as a italic key combination.
   this->addIgnoreKeyCombination({ Qt::ControlModifier, 'i' });
   this->addIgnoreKeyCombination({ Qt::ControlModifier, 'I' });

   connect(this->document(), SIGNAL(contentsChange(int, int, int)), this, SLOT(documentContentsChange(int,int,int)));
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

void ChatTextEdit::documentContentsChange(int position, int charsRemoved, int charsAdded)
{
   if (charsAdded > 0)
   {
      if (this->document()->characterAt(position).isSpace())
      {
         int i = position - 1;
         QString word;
         while (i > 0 && !this->document()->characterAt(i).isSpace())
            word.prepend(this->document()->characterAt(i--));
         if (!word.isEmpty())
            emit wordTyped(i + 1, word);
      }
   }
}
