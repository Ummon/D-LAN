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

#include <RemoteBrowseDialog/PathCompletionBox.h>
using namespace GUI;

#include <QKeyEvent>

namespace
{
   constexpr int MAX_NB_DISPLAYED_PATHS = 10;
}

/**
  * @class GUI::PathCompletionBox
  *
  * The paths proposed to complete the one being typed in a text box, displayed right below it and over the other
  * widgets of 'parent'. It behaves as the one of the location bar of Dolphin (KDE):
  *  - The text box keeps the focus. Up, down, tab and shift+tab go through the proposals, the text box then holds
  *    the highlighted one.
  *  - Tab takes the proposal if there is only one: the paths completing it are then proposed.
  *  - Enter chooses the highlighted proposal, the content of a directory is then proposed.
  *  - Escape gives the typed path back and closes the box.
  * The content of a directory is also proposed when the user puts the text cursor at the end of its path.
  */

PathCompletionBox::PathCompletionBox(QLineEdit* lineEdit, QWidget* parent) :
   QListView(parent), lineEdit(lineEdit)
{
   this->setModel(&this->paths);
   this->setFocusPolicy(Qt::NoFocus);
   this->setEditTriggers(QAbstractItemView::NoEditTriggers);
   this->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
   // The paths differ by their end.
   this->setTextElideMode(Qt::ElideLeft);
   this->setUniformItemSizes(true);
   this->hide();

   this->lineEdit->installEventFilter(this);
   connect(this->lineEdit, &QLineEdit::textChanged, this, [this](const QString& text) {
      // The proposals aren't the ones of another path.
      if (text != this->typedPath && !this->isBrowsing())
         this->hide();
   });
   connect(this, &QListView::clicked, this, [this](const QModelIndex& index) { this->take(index.row(), false); });
}

/**
  * Proposes the given completions of 'path', the ones received too late are ignored.
  */
void PathCompletionBox::setCompletions(const QString& path, const QStringList& completions)
{
   if (path != this->lineEdit->text() || !this->lineEdit->hasFocus())
      return;

   // There is nothing to propose for a path which is already complete.
   if (completions.isEmpty() || (completions.size() == 1 && completions.constFirst() == path))
   {
      this->hide();
      return;
   }

   this->typedPath = path;
   this->paths.setStringList(completions);
   this->scrollToTop();
   this->place();
   this->show();
   this->raise();
}

/**
  * Returns 'true' if the text box holds the highlighted proposal: the user is going through them and may choose
  * another one.
  */
bool PathCompletionBox::isBrowsing() const
{
   return this->isVisible() && this->currentIndex().isValid() &&
      this->currentIndex().data().toString() == this->lineEdit->text();
}

bool PathCompletionBox::eventFilter(QObject* watched, QEvent* event)
{
   if (watched == this->lineEdit && this->isVisible())
      switch (event->type())
      {
      case QEvent::KeyPress:
         if (this->keyPressed(static_cast<QKeyEvent*>(event)))
            return true;
         break;

      case QEvent::FocusOut:
         this->dismiss(false);
         break;

      case QEvent::Move:
      case QEvent::Resize:
         this->place();
         break;

      default:
         break;
      }
   else if (watched == this->lineEdit && this->cursorPutAtEnd(event))
      emit completionsWanted(this->lineEdit->text());

   return QListView::eventFilter(watched, event);
}

/**
  * Returns 'true' if the user has just put the text cursor at the end of the path of a directory, after its slash.
  * The cursor is moved when a mouse button or a key is pressed, it's thus looked at when they are released.
  */
bool PathCompletionBox::cursorPutAtEnd(QEvent* event) const
{
   switch (event->type())
   {
   case QEvent::MouseButtonRelease:
      if (static_cast<QMouseEvent*>(event)->button() != Qt::LeftButton)
         return false;
      break;

   case QEvent::KeyRelease:
      {
         const QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
         if (keyEvent->isAutoRepeat() || (keyEvent->key() != Qt::Key_End && keyEvent->key() != Qt::Key_Right))
            return false;
      }
      break;

   default:
      return false;
   }

   const QString path = this->lineEdit->text();
   return
      this->lineEdit->hasFocus() && !this->lineEdit->hasSelectedText() &&
      this->lineEdit->cursorPosition() == path.size() && (path.endsWith('/') || path.endsWith('\\'));
}

/**
  * Returns 'true' if the key was for the proposals, the text box must then ignore it.
  */
bool PathCompletionBox::keyPressed(QKeyEvent* event)
{
   const int count = this->paths.rowCount();
   const int row = this->currentIndex().row();
   // There is at least one proposal while the box is shown.
   const int previous = row > 0 ? row - 1 : count - 1;
   const int next = (row + 1) % count;
   const auto page = [this] { return qMax(1, this->viewport()->height() / this->sizeHintForRow(0)); };

   switch (event->key())
   {
   case Qt::Key_Tab:
   case Qt::Key_Backtab:
      if (event->modifiers() & ~Qt::ShiftModifier)
         return false;
      if (event->key() == Qt::Key_Backtab || event->modifiers() == Qt::ShiftModifier)
         this->browse(previous);
      else if (count == 1)
         this->take(0, true);
      else
         this->browse(next);
      return true;

   case Qt::Key_Down:
      this->browse(next);
      return true;

   case Qt::Key_Up:
      this->browse(previous);
      return true;

   case Qt::Key_PageDown:
      this->browse(qMin(row + page(), count - 1));
      return true;

   case Qt::Key_PageUp:
      this->browse(qMax(row - page(), 0));
      return true;

   case Qt::Key_Return:
   case Qt::Key_Enter:
      // Without any highlighted proposal the typed path is validated as if there were no proposals.
      if (!this->isBrowsing())
         return false;
      // The content of a chosen directory is then proposed, there is nothing more to propose for a file.
      this->take(row, this->lineEdit->text().endsWith('/'));
      return true;

   case Qt::Key_Escape:
      this->dismiss(true);
      return true;

   default:
      return false;
   }
}

/**
  * Highlights a proposal and puts it in the text box, it isn't chosen yet.
  */
void PathCompletionBox::browse(int row)
{
   const QModelIndex index = this->paths.index(row);
   this->setCurrentIndex(index);
   this->lineEdit->setText(index.data().toString());
   emit pathEdited(this->lineEdit->text());
}

/**
  * Puts a proposal in the text box as if it has been typed and closes the box.
  * @param completed The paths completing the proposal are wanted.
  */
void PathCompletionBox::take(int row, bool completed)
{
   const QString path = this->paths.index(row).data().toString();
   this->hide();
   this->lineEdit->setText(path);
   emit pathEdited(path);
   if (completed)
      emit completionsWanted(path);
}

/**
  * Closes the box. The highlighted proposal held by the text box becomes the chosen path, unless the typed one
  * is restored.
  */
void PathCompletionBox::dismiss(bool restoreTypedPath)
{
   const bool browsing = this->isBrowsing();
   this->hide();
   if (!browsing)
      return;

   if (restoreTypedPath)
      this->lineEdit->setText(this->typedPath);
   emit pathEdited(this->lineEdit->text());
}

/**
  * Places the box right below the text box with the same width, its height depends on the number of proposals.
  */
void PathCompletionBox::place()
{
   const QPoint position = this->lineEdit->mapTo(this->parentWidget(), QPoint(0, this->lineEdit->height()));
   const int height =
      qMin(this->paths.rowCount(), MAX_NB_DISPLAYED_PATHS) * this->sizeHintForRow(0) + 2 * this->frameWidth();
   this->setGeometry(
      position.x(), position.y(), this->lineEdit->width(), qMin(height, this->parentWidget()->height() - position.y())
   );
}
