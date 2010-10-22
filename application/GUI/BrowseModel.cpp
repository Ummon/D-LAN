#include <BrowseModel.h>
using namespace GUI;

BrowseModel::BrowseModel(CoreConnection& coreConnection, const Common::Hash& peerID) :
    coreConnection(coreConnection), peerID(peerID)
{
   this->browseResult = coreConnection.browse(this->peerID);
   connect(this->browseResult.data(), SIGNAL(result(const Protos::Common::Entries&)), this, SLOT(result(const Protos::Common::Entries&)));
   this->browseResult->start();
}

QModelIndex BrowseModel::index(int row, int column, const QModelIndex &parent) const
{
   return QModelIndex();
}

QModelIndex BrowseModel::parent(const QModelIndex& child) const
{
   return QModelIndex();
}

int BrowseModel::rowCount(const QModelIndex& parent) const
{
   return 0;
}

int BrowseModel::columnCount(const QModelIndex& parent) const
{
   return 0;
}

QVariant BrowseModel::data(const QModelIndex& index, int role) const
{
   return QVariant();
}

void BrowseModel::result(const Protos::Common::Entries& entries)
{
   this->browseResult.clear();
}

