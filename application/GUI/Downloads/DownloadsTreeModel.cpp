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
  
#include <Downloads/DownloadsTreeModel.h>
using namespace GUI;

#include <Common/ProtoHelper.h>

DownloadsTreeModel::DownloadsTreeModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter) :
   DownloadsModel(coreConnection, peerListModel, sharedDirsModel, filter), root(new Node())
{
}

DownloadsTreeModel::~DownloadsTreeModel()
{

}

quint64 DownloadsTreeModel::getDownloadID(const QModelIndex& index) const
{

}

bool DownloadsTreeModel::isDownloadPaused(const QModelIndex& index) const
{

}

bool DownloadsTreeModel::isFileLocationKnown(const QModelIndex& index) const
{

}

bool DownloadsTreeModel::isFileComplete(const QModelIndex& index) const
{

}

QString DownloadsTreeModel::getPath(const QModelIndex& index, bool appendFilename) const
{

}

int DownloadsTreeModel::rowCount(const QModelIndex& parent) const
{

}

int DownloadsTreeModel::columnCount(const QModelIndex& parent) const
{

}

QVariant DownloadsTreeModel::data(const QModelIndex& index, int role) const
{

}

QModelIndex DownloadsTreeModel::index(int row, int column, const QModelIndex& parent) const
{
   if (!this->hasIndex(row, column, parent))
       return QModelIndex();

   const Node* parentNode;

    if (!parent.isValid())
       parentNode = this->root;
   else
       parentNode = static_cast<Node*>(parent.internalPointer());

   Node* childNode = parentNode->getChild(row);

   if (childNode)
       return this->createIndex(row, column, childNode);

   return QModelIndex();
}

QModelIndex DownloadsTreeModel::parent(const QModelIndex& child) const
{

}

Qt::DropActions DownloadsTreeModel::supportedDropActions() const
{

}

Qt::ItemFlags DownloadsTreeModel::flags(const QModelIndex& index) const
{

}

bool DownloadsTreeModel::dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex& parent)
{

}

void DownloadsTreeModel::onNewState(const Protos::GUI::State& state)
{
   QList<int> activeDownloadIndices = this->getNonFilteredDownloadIndices(state);

   for (int i = 0; i < activeDownloadIndices.size(); i++)
   {
      const Protos::GUI::State::Download& download = state.download(activeDownloadIndices[i]);
      // TODO
   }
}

/////

DownloadsTreeModel::Node::Node() :
   toDelete(false), parent(0)
{
}

DownloadsTreeModel::Node::Node(const Protos::GUI::State::Download& download, Node* parent) :
   toDelete(false), parent(parent), download(download)
{
}

DownloadsTreeModel::Node::~Node()
{
}

DownloadsTreeModel::Node* DownloadsTreeModel::Node::getChild(int row) const
{
   if (row >= this->children.size())
      return 0;
   return this->children[row];
}

/////

bool GUI::operator>(const Protos::GUI::Download& d1, const Protos::GUI::Download& d2)
{
   if (d1.entry().type() != d2.entry().type())
      return d1.entry().type() < d2.entry().type();
   return Common::ProtoHelper::getStr(d1.entry(), &Protos::Common::Entry::name) > Common::ProtoHelper::getStr(d2.entry(), &Protos::Common::Entry::name);
}

bool GUI::operator<(const Protos::GUI::Download& d1, const Protos::GUI::Download& d2)
{
   if (d1.entry().type() != d2.entry().type())
      return d1.entry().type() > d2.entry().type();
   return Common::ProtoHelper::getStr(d1.entry(), &Protos::Common::Entry::name) < Common::ProtoHelper::getStr(d2.entry(), &Protos::Common::Entry::name);
}
