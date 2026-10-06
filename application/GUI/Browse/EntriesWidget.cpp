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

#include <Browse/EntriesWidget.h>
using namespace GUI;

#include <QKeyEvent>
#include <QSet>

#include <Utils.h>

/**
  * @class GUI::EntriesWidget
  *
  * The base of the widgets showing the entries of some peers in a tree view: 'BrowseWidget' and 'SearchWidget'.
  * What can be done with the selected entries is the same for both: download them, open their location or open them.
  */

EntriesWidget::EntriesWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QWidget(parent), coreConnection(coreConnection)
{
}

/**
  * Must be called by the sub-class before anything else, once its model and its view exist.
  */
void EntriesWidget::setEntries(const BrowseModel* model, const QTreeView* view)
{
   this->model = model;
   this->view = view;
}

/**
  * Returns 'false' for an entry which doesn't have a location on its own, it can neither be opened nor located.
  */
bool EntriesWidget::hasOwnLocation(const QModelIndex&) const
{
   return true;
}

void EntriesWidget::openFile(const QModelIndex& index) const
{
   if (
      this->coreConnection->isLocal() &&
      this->hasOwnLocation(index) &&
      this->coreConnection->getRemoteID() == this->entryPeerID(index) &&
      !this->model->isDir(index)
   )
      Utils::openFile(this->model->getPath(index));
}

void EntriesWidget::keyPressEvent(QKeyEvent* event)
{
   // Return key -> open all selected files.
   if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter)
   {
      for (const QModelIndex& index : this->selectedRows())
         this->openFile(index);
   }
   else
      QWidget::keyPressEvent(event);
}

/**
  * Download all selected items to the first available directory.
  * If not directory is available then ask the user to choose one.
  */
void EntriesWidget::download()
{
   if (this->model->nbSharedDirs() == 0)
      this->downloadTo();
   else
      for (const QModelIndex& index : this->selectedRows())
         this->coreConnection->download(this->entryPeerID(index), this->model->getEntry(index));
}

/**
  * Ask the user to chose a directory and download all selected items into it.
  */
void EntriesWidget::downloadTo()
{
   Utils::askForADirectoryToDownloadTo(this, this->coreConnection, [this](const QString& dir) { this->downloadTo(dir); });
}

/**
  * Download all selected items to 'path'.
  */
void EntriesWidget::downloadTo(const Common::Path& path)
{
   for (const QModelIndex& index : this->selectedRows())
      this->coreConnection->download(this->entryPeerID(index), this->model->getEntry(index), path);
}

/**
  * Download all selected items to a folder of the shared directory.
  * @param relativePath The folder relative to the shared directory, empty for the shared directory itself.
  */
void EntriesWidget::downloadTo(const Common::Hash& sharedDirID, const Common::Path& relativePath)
{
   for (const QModelIndex& index : this->selectedRows())
      this->coreConnection->download(this->entryPeerID(index), this->model->getEntry(index), sharedDirID, relativePath);
}

void EntriesWidget::openLocation()
{
   QSet<QString> locations;
   for (const QModelIndex& index : this->selectedRows())
      if (this->hasOwnLocation(index))
         locations.insert(this->model->getPath(index, true));

   Utils::openLocations(locations.values(), this);
}

QModelIndexList EntriesWidget::selectedRows() const
{
   return this->view->selectionModel()->selectedRows();
}
