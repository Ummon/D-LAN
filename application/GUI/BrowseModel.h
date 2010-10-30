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
   protected:
      class Node;

   private:
      Q_OBJECT

   public:
      BrowseModel(CoreConnection& coreConnection, const Common::Hash& peerID);
      virtual ~BrowseModel();

      virtual QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const;
      virtual QModelIndex parent(const QModelIndex& child) const;
      virtual int rowCount(const QModelIndex& parent = QModelIndex()) const;
      virtual int columnCount(const QModelIndex& parent = QModelIndex()) const;
      virtual QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      virtual Protos::Common::Entry getEntry(const QModelIndex& index);

   protected slots:
      virtual void result(const Protos::Common::Entries& entries);

   protected:
      virtual void browse(const Common::Hash& peerID, Node* node = 0);
      virtual void loadChildren(const QPersistentModelIndex &index);

      class Node
      {
      public:
         Node();
         Node(const Protos::Common::Entry& entry, Node* parent);
         virtual ~Node();
         virtual Node* getChild(int row);
         virtual Node* getParent();
         virtual int getNbChildren() const;
         virtual int getRow() const;
         virtual QVariant getData(int column) const;
         virtual void insertChildren(const Protos::Common::Entries& entries);
         virtual bool hasUnloadedChildren();
         virtual const Protos::Common::Entry& getEntry() const;

      protected:
         virtual Node* newNode(const Protos::Common::Entry& entry);

         Protos::Common::Entry entry;
         Node* parent;
         QList<Node*> children;
      };

      CoreConnection& coreConnection;
      Common::Hash peerID;

      QPersistentModelIndex currentBrowseIndex; // When we receive some entries after a browse query, they will be added as children to this index.
      QSharedPointer<IBrowseResult> browseResult;

      Node* root;
   };
}

#endif
