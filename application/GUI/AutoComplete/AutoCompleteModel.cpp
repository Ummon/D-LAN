#include <AutoComplete/AutoCompleteModel.h>
using namespace GUI;

AutoCompleteModel::AutoCompleteModel()
{
}

AutoCompleteModel::~AutoCompleteModel()
{

}

void AutoCompleteModel::setFilter(const QString& pattern)
{

}

QModelIndex AutoCompleteModel::index(int row, int column, const QModelIndex& parent) const
{
   return QModelIndex();
}

QModelIndex AutoCompleteModel::parent(const QModelIndex& child) const
{
   return QModelIndex();
}

int AutoCompleteModel::rowCount(const QModelIndex& parent) const
{
   return 0;
}

int AutoCompleteModel::columnCount(const QModelIndex& parent) const
{
   return 0;
}

QVariant AutoCompleteModel::data(const QModelIndex& index, int role) const
{
   return QVariant();
}
