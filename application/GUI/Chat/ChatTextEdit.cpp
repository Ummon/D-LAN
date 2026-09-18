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

   const bool keyPress = e->type() == QEvent::KeyPress;
   const int revision = this->document()->revision();
   const bool handled = QTextEdit::event(e);
   // Completion needs the cursor position after the editor has processed the key.
   if (keyPress && this->document()->revision() != revision)
      emit textEdited();
   return handled;
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
      // A paste can insert several words, including leading whitespace and line breaks.
      // Include words crossing the edit boundaries, then replace from right to left so
      // shrinking a word to an image cannot invalidate the remaining positions.
      const auto isBoundary = [&](int index) {
         return documentText.at(index).isSpace() || documentText.at(index) == QChar::ObjectReplacementCharacter;
      };
      int begin = qMin(position, int(documentText.size()));
      int end = qMin(position + charsAdded, int(documentText.size()));
      while (begin > 0 && !isBoundary(begin - 1))
         --begin;
      while (end < documentText.size() && !isBoundary(end))
         ++end;

      while (end > begin)
      {
         while (end > begin && isBoundary(end - 1))
            --end;
         const int wordEnd = end;
         while (end > begin && !isBoundary(end - 1))
            --end;
         if (end < wordEnd)
            emit wordTyped(end, documentText.mid(end, wordEnd - end));
      }
      // A receiver may synchronously replace words with emoticons.
      this->previousDocumentText = this->document()->toRawText();
   }
}
