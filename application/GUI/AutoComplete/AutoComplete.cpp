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

#include <Log.h>

AutoComplete::AutoComplete(QWidget* parent) :
   QWidget(parent),
   ui(new Ui::AutoComplete)
{
   this->ui->setupUi(this);

   this->filterModel.setFilterCaseSensitivity(Qt::CaseInsensitive);

   this->filterModel.setSourceModel(&this->model);
   this->ui->listView->setModel(&this->filterModel);

   this->ui->listView->installEventFilter(this);
}

void AutoComplete::setValues(const QList<QPair<Common::Hash, QString>>& values)
{
   this->model.setValues(values);

   if (this->filterModel.rowCount() > 0)
      this->ui->listView->selectionModel()->select(this->filterModel.index(0, 0), QItemSelectionModel::Select);
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
   // if (event->modifiers() == Qt::NoModifier)

   if (obj == this->ui->listView && event->type() == QEvent::KeyPress)
   {
      QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);

      L_DEBU(QString("AutoComplete::eventFilter, key: %1, modifier: %2, text: %3").arg(keyEvent->key()).arg(keyEvent->modifiers()).arg(keyEvent->text()));

      switch (keyEvent->key())
      {
      case Qt::Key_Backspace:
         if (!this->currentPattern.isEmpty())
         {
            this->currentPattern.remove(this->currentPattern.size() - 1, 1);
            this->filterModel.setFilterWildcard(this->currentPattern + "*");
            emit lastCharRemoved();
         }
         break;

      case Qt::Key_Escape:
         this->currentPattern.clear();
         this->model.setValues(QList<QPair<Common::Hash, QString>>());
         this->close();
         break;

      case Qt::Key_Enter:
      case Qt::Key_Return:
      case Qt::Key_Space:
         this->close();
         break;

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
   this->currentPattern.clear();
   this->filterModel.setFilterWildcard("");
   this->ui->listView->setFocus();
}
