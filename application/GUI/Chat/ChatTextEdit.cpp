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
  
#include <GUI/Chat/ChatTextEdit.h>

#include <QKeyEvent>

#include <Log.h>

using namespace GUI;

ChatTextEdit::ChatTextEdit(QWidget* parent) :
   QTextEdit(parent)
{
   // Ignore CTRL-I, we use it as a italic key combination.
   this->addIgnoreKeyCombination({ Qt::ControlModifier, 'i' });
   this->addIgnoreKeyCombination({ Qt::ControlModifier, 'I' });

   connect(this->document(), SIGNAL(contentsChange(int, int, int)), this, SLOT(documentContentsChange(int, int, int)));
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
      if (!this->document()->characterAt(position).isSpace())
      {
         int i = position + charsAdded - 1;
         if (this->document()->characterAt(i).isSpace())
            i--;

         QString word;
         while (i >= 0 && !this->document()->characterAt(i).isSpace())
            word.prepend(this->document()->characterAt(i--));
         if (!word.isEmpty())
            emit wordTyped(i + 1, word);
      }
   }
}
