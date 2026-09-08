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
   this->addIgnoreKeyCombination({ Qt::ControlModifier, Qt::Key_I });

   connect(this->document(), &QTextDocument::undoCommandAdded, this, [this]() {
      this->previousUndoSteps = this->document()->availableUndoSteps();
   });
   connect(this->document(), &QTextDocument::contentsChange, this, &ChatTextEdit::documentContentsChange);
}

void ChatTextEdit::setEmoticons(const Emoticons* emoticons)
{
   this->emoticons = emoticons;
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

QVariant ChatTextEdit::loadResource(int type, const QUrl& name)
{
   if (this->emoticons && type == QTextDocument::ImageResource && name.scheme() == "emoticons")
      return this->emoticons->getSmileImage(name.host(), name.path().mid(1)); // Skip the '/' at the beginning.
   return QTextEdit::loadResource(type, name);
}


void ChatTextEdit::documentContentsChange(int position, int charsRemoved, int charsAdded)
{
   // Qt reports formatting as removed/reinserted characters too; only actual text edits are typing.
   const QString documentText = this->document()->toRawText();
   const bool textChanged = documentText != this->previousDocumentText;
   this->previousDocumentText = documentText;

   // New edits announce their undo commands first; undo/redo moves through existing commands.
   // Merged typing keeps the same step count and must still be checked for emoticons.
   const int undoSteps = this->document()->availableUndoSteps();
   const bool undoOrRedo = this->document()->isUndoRedoEnabled() && undoSteps != this->previousUndoSteps;
   this->previousUndoSteps = undoSteps;
   if (textChanged && charsAdded > 0 && !undoOrRedo)
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
         {
            emit wordTyped(i + 1, word);
            // A receiver may synchronously replace the word with an emoticon.
            this->previousDocumentText = this->document()->toRawText();
         }
      }
   }
}
