#ifndef GUI_BROWSEMODEL_H
#define GUI_BROWSEMODEL_H

#include <QAbstractItemModel>
#include <QVariant>

#include <Common/ProtoHelper.h>

#include <CoreConnection.h>

namespace GUI
{
   class BrowseModel : public QAbstractItemModel
   {
      Q_OBJECT
   public:
      BrowseModel(CoreConnection& coreConnection, const Common::Hash& peerID);
      ~BrowseModel();

      QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const;
      QModelIndex parent(const QModelIndex& child) const;
      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      Protos::Common::Entry getEntry(const QModelIndex& index);

   private slots:
      void result(const Protos::Common::Entries& entries);

   private:
      void loadChildren(const QPersistentModelIndex &index);

      CoreConnection& coreConnection;
      const Common::Hash peerID;
      quint64 tag;

      QPersistentModelIndex currentBrowseIndex;
      QSharedPointer<BrowseResult> browseResult;

      class Node
      {
      public:
         Node();
         Node(const Protos::Common::Entry& entry, Node* parent);
         ~Node();
         Node* getChild(int row);
         Node* getParent();
         int getNbChildren() const;
         int getRow() const;
         QVariant getData(int column) const;
         void insertChildren(const Protos::Common::Entries& entries);
         bool hasUnloadedChildren();
         const Protos::Common::Entry& getEntry() const;


      private:
         Protos::Common::Entry entry;
         Node* parent;
         QList<Node*> children;
      };
      Node* root;
   };
}

#endif
