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
  
#include <AutoComplete/AutoComplete.h>
#include <ui_AutoComplete.h>
using namespace GUI;

#include <QKeyEvent>
#include <QScreen>
#include <QScrollBar>

#include <Log.h>

AutoComplete::AutoComplete(QWidget* parent) :
   QWidget(parent),
   ui(new Ui::AutoComplete),
   validated(false)
{
   this->ui->setupUi(this);

   this->filterModel.setFilterCaseSensitivity(Qt::CaseInsensitive);

   this->filterModel.setSourceModel(&this->model);
   this->ui->listView->setModel(&this->filterModel);

   this->ui->listView->installEventFilter(this);

   // The next three 'connects' are here to automatically select the first list item when the list is populated and was previously empty.
   auto selectFirstItem = [this](){
      if (this->ui->listView->selectionModel()->selectedRows().isEmpty() && this->filterModel.rowCount() > 0)
         this->ui->listView->selectionModel()->setCurrentIndex(this->filterModel.index(0, 0), QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows);
   };
   connect(&this->filterModel, &QSortFilterProxyModel::rowsInserted, selectFirstItem);
   connect(&this->filterModel, &QSortFilterProxyModel::rowsRemoved, selectFirstItem);
   connect(&this->filterModel, &QSortFilterProxyModel::modelReset, [this](){
      if (this->filterModel.rowCount() > 0)
         this->ui->listView->selectionModel()->setCurrentIndex(this->filterModel.index(0, 0), QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows);
   });

   connect(&this->filterModel, &QSortFilterProxyModel::rowsInserted, this, &AutoComplete::updateHeight);
   connect(&this->filterModel, &QSortFilterProxyModel::rowsRemoved, this, &AutoComplete::updateHeight);
   connect(&this->filterModel, &QSortFilterProxyModel::modelReset, this, &AutoComplete::updateHeight);
   connect(&this->filterModel, &QSortFilterProxyModel::dataChanged, this, &AutoComplete::updateHeight);
   connect(this->ui->listView->horizontalScrollBar(), &QScrollBar::rangeChanged,
      this, &AutoComplete::updateHeight, Qt::QueuedConnection);
}

void AutoComplete::setValues(const QList<QPair<Common::Hash, QString>>& values)
{
   this->model.setValues(values);
}

/**
  * Returns the current selected hash. It may return a null hash if nothing is selected.
  */
Common::Hash AutoComplete::getCurrent() const
{
   auto selection = this->ui->listView->selectionModel()->selectedRows();
   if (selection.size() > 0)
      return this->model.getHash(this->filterModel.mapToSource(selection[0]));
   return Common::Hash();
}

bool AutoComplete::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->ui->listView && event->type() == QEvent::KeyPress)
   {
      QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);

      /* L_DEBU(
         QString("AutoComplete::eventFilter, key: %1, modifier: %2, text: %3")
            .arg(keyEvent->key()).arg(keyEvent->modifiers()).arg(keyEvent->text())
      );
      */

      switch (keyEvent->key())
      {
      case Qt::Key_Backspace:
         if (!this->currentPattern.isEmpty())
         {
            // QString and document positions count UTF-16 units; keep surrogate pairs intact.
            const int length = this->currentPattern.size();
            const int charsRemoved = length >= 2 &&
               this->currentPattern.at(length - 1).isLowSurrogate() &&
               this->currentPattern.at(length - 2).isHighSurrogate() ? 2 : 1;
            this->currentPattern.chop(charsRemoved);
            this->filterModel.setFilterWildcard(this->currentPattern + "*");
            emit lastCharRemoved(charsRemoved);
         }
         else
            this->close();
         break;

      case Qt::Key_Escape:
         this->close();
         break;

      // This is the only was to validate the selected entry.
      case Qt::Key_Enter:
      case Qt::Key_Return:
      case Qt::Key_Space:
         this->validated = true;
         this->close();
         break;

      case Qt::Key_Up:
      case Qt::Key_Down:
         return QWidget::eventFilter(obj, event);

      default:
         if (keyEvent->key() < Qt::Key_Escape)
         {
            const QString& text = keyEvent->text();
            if (!text.isEmpty())
            {
               this->currentPattern.append(text);
               this->filterModel.setFilterWildcard(this->currentPattern + "*");
               emit stringAdded(text);
            }
         }
         break;
      }

      return true;
   }

   return QWidget::eventFilter(obj, event);
}

void AutoComplete::showEvent(QShowEvent* event)
{
   this->reset();
   this->ui->listView->setFocus();
}

void AutoComplete::closeEvent(QCloseEvent*)
{
   if (!this->validated)
      this->reset();

   emit closed();
}

void AutoComplete::reset()
{
   this->validated = false;
   this->model.setValues(QList<QPair<Common::Hash, QString>>());
   this->currentPattern.clear();
   this->filterModel.setFilterWildcard("");
}

void AutoComplete::updateHeight()
{
   QListView* list = this->ui->listView;
   list->doItemsLayout();

   const QRect available = this->screen()->availableGeometry();
   const QMargins margins = this->layout()->contentsMargins();
   int height = margins.top() + margins.bottom() + 2 * list->frameWidth();
   if (list->horizontalScrollBar()->maximum() > 0)
      height += list->horizontalScrollBar()->sizeHint().height();

   // Keep an empty list usable while typing a pattern with no matches.
   if (this->filterModel.rowCount() == 0)
      height += list->fontMetrics().height();
   for (int row = 0; row < this->filterModel.rowCount() && height < available.height(); ++row)
      height += list->sizeHintForRow(row) + 2 * list->spacing();

   this->setFixedHeight(qMin(height, available.height()));
   if (this->isWindow())
      this->move(this->x(), qBound(available.top(), this->y(), available.bottom() - this->height() + 1));
}
