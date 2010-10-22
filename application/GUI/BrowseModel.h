#ifndef GUI_BROWSEMODEL_H
#define GUI_BROWSEMODEL_H

#include <QAbstractItemModel>

#include <CoreConnection.h>

namespace GUI
{
   class BrowseModel : public QAbstractItemModel
   {
      Q_OBJECT
   public:
      BrowseModel(CoreConnection& coreConnection, const Common::Hash& peerID);

      QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const;
      QModelIndex parent(const QModelIndex& child) const;
      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   signals:

   public slots:

   private slots:
      void result(const Protos::Common::Entries& entries);

   private:
      CoreConnection& coreConnection;
      const Common::Hash& peerID;
      quint64 tag;

      QSharedPointer<BrowseResult> browseResult;
   };
}

#endif
