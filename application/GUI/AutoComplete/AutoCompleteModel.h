#ifndef GUI_AUTOCOMPLETEMODEL_H
#define GUI_AUTOCOMPLETEMODEL_H

#include <QAbstractItemModel>

#include <Common/Hash.h>

namespace GUI
{
   class AutoCompleteModel : public QAbstractItemModel
   {
      Q_OBJECT

   public:
      AutoCompleteModel();
      ~AutoCompleteModel();

      void setFilter(const QString& pattern);

      QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
      QModelIndex parent(const QModelIndex& child) const;
      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   private:
      QList<QPair<Common::Hash, QString>> values;
   };
}

#endif
