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

#include <RemoteBrowseDialog/RemoteBrowseModel.h>
using namespace GUI;

#include <algorithm>

#include <QPixmap>
#include <QFileInfo>

#include <IconProvider.h>

#include <Common/Global.h>

#include <Log.h>

/**
  * @class GUI::BrowseModel
  *
  * The model of a distant peer file system. The directory content is lazy loaded, see the method 'loadChildren()'.
  * Used by 'WidgetBrowse'.
  */

RemoteBrowseModel::RemoteBrowseModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection), filters(FILE | DIR), currentTreeExploring(nullptr), root(new Tree())
{
   this->browse(this->root);
}

RemoteBrowseModel::~RemoteBrowseModel()
{
   if (!this->localBrowseResult.isNull())
      this->localBrowseResult->disconnect(this);

   delete this->root;
}

bool RemoteBrowseModel::isLocal() const
{
   return this->coreConnection->isLocal();
}

QModelIndex RemoteBrowseModel::index(int row, int column, const QModelIndex& parent) const
{
   if (!this->hasIndex(row, column, parent))
      return QModelIndex();

   const Tree* parentTree;

   if (!parent.isValid())
      parentTree = this->root;
   else
      parentTree = static_cast<Tree*>(parent.internalPointer());

   Tree* childTree = parentTree->getChild(row);

   if (childTree)
      return this->createIndex(row, column, childTree);
   else if (parentTree->hasUnloadedChildren()) // The view want some not yet loaded children . . . so we will load them.
      const_cast<RemoteBrowseModel*>(this)->loadChildren(parent);

   return QModelIndex();
}

QModelIndex RemoteBrowseModel::parent(const QModelIndex& index) const
{
   if (!index.isValid())
      return QModelIndex();

   Tree* tree = static_cast<Tree*>(index.internalPointer());
   Tree* parentItem = tree->getParent();

   if (!parentItem || parentItem == this->root)
      return QModelIndex();

   return this->createIndex(parentItem->getOwnPosition(), 0, parentItem);
}

int RemoteBrowseModel::rowCount(const QModelIndex& parent) const
{
   if (parent.column() > 0)
      return 0;

   const Tree* parentTree;

   if (!parent.isValid())
      parentTree = this->root;
   else
      parentTree = static_cast<Tree*>(parent.internalPointer());

   int nbLoadedChildren = parentTree->getNbChildren();
   if (nbLoadedChildren > 0)
      return nbLoadedChildren;

   if (parentTree->hasUnloadedChildren())
      return 1; // We lie and tell there is a child.
   else
      return 0;
}

int RemoteBrowseModel::columnCount(const QModelIndex&) const
{
   return 3;
}

QVariant RemoteBrowseModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      {
         Tree* tree = static_cast<Tree*>(index.internalPointer());
         return tree->data(index.column());
      }

   case Qt::DecorationRole:
      {
         if (index.column() == 0)
         {
            Tree* tree = static_cast<Tree*>(index.internalPointer());
            const auto& item = tree->getItem();
            QString name = QString::fromStdString(item.name());
            if (item.type() == Protos::GUI::LocalBrowseResult::DIR)
               name += "/";
            return IconProvider::getIcon(Common::Path(name));
         }
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return QVariant((index.column() == NAME ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter);

   default:
      return QVariant();
   }
}

void RemoteBrowseModel::setFilters(Filters filters)
{
   this->filters = filters;
}

/**
  * Returns the local path of the entry at the given index.
  */
QString RemoteBrowseModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   return static_cast<Tree*>(index.internalPointer())->path();
}

void RemoteBrowseModel::getIndexFromPath(const QString& path)
{
   this->directoriesToExplore = QDir::cleanPath(path).split('/');
   this->currentTreeExploring = this->root;
   this->exploreDirectories();
}

void RemoteBrowseModel::result(const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries)
{
   google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry> sortedEntries;
   for (const auto& entry : entries)
   {
      if (
         entry.type() == Protos::GUI::LocalBrowseResult::DIR && this->filters.testAnyFlag(DIR) ||
         entry.type() == Protos::GUI::LocalBrowseResult::FILE && this->filters.testAnyFlag(FILE)
      )
         sortedEntries.Add()->CopyFrom(entry);
   }

   std::sort(
      sortedEntries.begin(),
      sortedEntries.end(),
      [](const auto& e1, const auto& e2)
      {
         if (e1.type() != e2.type())
            return e1.type() == Protos::GUI::LocalBrowseResult::DIR;
         return e1.name() < e2.name();
      }
   );

   if (sortedEntries.size() > 0)
   {
      this->beginInsertRows(this->currentBrowseIndex, 0, sortedEntries.size() - 1);

      if (this->currentBrowseIndex.internalPointer())
         static_cast<Tree*>(this->currentBrowseIndex.internalPointer())->insertChildren(sortedEntries);
      else
         this->root->insertChildren(sortedEntries);

      this->endInsertRows();
   }

   this->currentBrowseIndex = QModelIndex();
   this->localBrowseResult.clear();

   this->exploreDirectories();
}

void RemoteBrowseModel::resultTimeout()
{
   L_WARN("Asking for local entries message timed out");
   this->currentBrowseIndex = QModelIndex();
   this->localBrowseResult.clear();
   this->currentTreeExploring = nullptr;
   this->directoriesToExplore.clear();
}

void RemoteBrowseModel::browse(Tree* tree)
{
   if (!this->localBrowseResult.isNull())
      this->localBrowseResult->disconnect();

   L_DEBU(QString("### Browse: %1").arg(tree->path()));

   this->localBrowseResult = this->coreConnection->localBrowse(tree->path());
   connect(this->localBrowseResult.data(), &RCC::ILocalBrowseResult::result, this, &RemoteBrowseModel::result);
   connect(this->localBrowseResult.data(), &Common::Timeoutable::timeout, this, &RemoteBrowseModel::resultTimeout);
   this->localBrowseResult->start();
}

void RemoteBrowseModel::loadChildren(const QPersistentModelIndex &index)
{
   if (index == this->currentBrowseIndex)
      return;

   this->currentBrowseIndex = index;
   this->browse(static_cast<Tree*>(index.internalPointer()));
}

void RemoteBrowseModel::exploreDirectories()
{
   if (!this->currentTreeExploring)
      return;


   L_DEBU(QString("### exploreDirectories: %1").arg(this->directoriesToExplore.join('/')));


   // int row = 0;
   while (!this->directoriesToExplore.empty())
   {
      const int nbChildren = this->currentTreeExploring->getNbChildren();

      // No children loaded -> we will load children with 'loadChildren()'.
      if (nbChildren == 0)
         break;

      const auto& directory = this->directoriesToExplore.first();
      bool directoryFound = false;

      for (int i = 0; i < nbChildren; i++)
      {
         const auto& name = QString::fromStdString(this->currentTreeExploring->getChild(i)->getItem().name());
         if (name == directory || name == directory + '/')
         {
            this->currentTreeExploring = this->currentTreeExploring->getChild(i);
            this->directoriesToExplore.removeFirst();
            // row = i;
            directoryFound = true;
            break;
         }
      }

      // The children are loaded but the directory isn't found, the exploration is aborted.
      if (!directoryFound)
      {
         this->currentTreeExploring = nullptr;
         this->directoriesToExplore.clear();
         return;
      }
   }

   // If the path is already loaded we emit the signal 'indexFromPath'.
   if (this->directoriesToExplore.empty())
   {
      const QModelIndex index = this->indexFromTree(this->currentTreeExploring);
      this->currentTreeExploring = nullptr;
      emit indexFromPath(index);
   }
   else
   {
      this->loadChildren(this->indexFromTree(this->currentTreeExploring));
   }
}

/**
  * Returns the index corresponding to the given tree, the root has no index: QModelIndex().
  * The position is asked to the tree itself, it can't be cached in 'exploreDirectories()' because the
  * exploration is resumed asynchronously each time a browse result is received.
  */
QModelIndex RemoteBrowseModel::indexFromTree(Tree* tree) const
{
   if (!tree || tree == this->root)
      return QModelIndex();

   return this->createIndex(tree->getOwnPosition(), 0, tree);
}

/////

/**
  * @class GUI::Tree
  *
  * Either a file or a directory in the tree view structure.
  */

RemoteBrowseModel::Tree::Tree()
{
   this->getItem().set_type(Protos::GUI::LocalBrowseResult::DIR);
}

RemoteBrowseModel::Tree::Tree(const Protos::GUI::LocalBrowseResult::Entry& entry, Tree* parent) :
   Common::Tree<Protos::GUI::LocalBrowseResult::Entry, RemoteBrowseModel::Tree>(entry, parent)
{
}

RemoteBrowseModel::Tree::~Tree()
{
}

void RemoteBrowseModel::Tree::insertChildren(
   const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries
)
{
   for (int i = 0; i < entries.size(); i++)
      this->insertChild(entries.at(i));
}

bool RemoteBrowseModel::Tree::hasUnloadedChildren() const
{
   return
      this->getItem().type() == Protos::GUI::LocalBrowseResult::DIR  &&
      this->getNbChildren() == 0 &&
      this->getItem().size() > 0;
}

QVariant RemoteBrowseModel::Tree::data(int column) const
{
   switch (column)
   {
   case NAME:
      {
         const auto& item = this->getItem();
         if (item.volume_label().empty())
            return QString::fromStdString(item.name());
         else
         {
            QString name = QString::fromStdString(item.name());
            if (name.size() >= 3 && (name.endsWith('/') || name.endsWith('\\')))
               name.removeLast();
            return QString(QString::fromStdString(item.volume_label()) + " (" + name + ")");
         }
      }

   case DATE_MODIFIED:
      {
         const auto& item = this->getItem();
         if (item.volume_label().empty() && item.date_modified() != 0)
            return QDateTime::fromMSecsSinceEpoch(this->getItem().date_modified());
         else
            return QVariant();
      }

   case SIZE:
      {
         const auto& item = this->getItem();
         if (item.type() == Protos::GUI::LocalBrowseResult::FILE)
            return Common::Global::formatByteSize(this->getItem().size());
         else if (this->getItem().capacity() != 0)
            return
               QString(
                  Common::Global::formatByteSize(this->getItem().capacity() - this->getItem().size()) + " " +
                  tr("free")
               );
         else
            return QVariant();
      }
   default: return QVariant();
   }
}

QString RemoteBrowseModel::Tree::path() const
{
   QString path;

   const Tree* current = this;
   while (current->getParent()) {
      const auto& item = current->getItem();
      const bool isDir = item.type() == Protos::GUI::LocalBrowseResult::DIR;

      QString name = QString::fromStdString(item.name());

      if (isDir && !name.endsWith('/'))
         path.prepend('/');

      path.prepend(name);

      current = current->getParent();
   }

   return path;
}