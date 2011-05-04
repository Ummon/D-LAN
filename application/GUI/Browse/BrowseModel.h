/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef GUI_BROWSEMODEL_H
#define GUI_BROWSEMODEL_H

#include <QAbstractItemModel>
#include <QVariant>

#include <Protos/common.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/RemoteCoreController/IBrowseResult.h>

#include <Settings/DirListModel.h>

namespace GUI
{
   class BrowseModel : public QAbstractItemModel
   {
   protected:
      class Node;

   private:
      Q_OBJECT

   public:
      BrowseModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const DirListModel& sharedDirsModel, const Common::Hash& peerID, bool loadRoots = true);
      virtual ~BrowseModel();

      virtual QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const;
      virtual QModelIndex parent(const QModelIndex& child) const;
      virtual int rowCount(const QModelIndex& parent = QModelIndex()) const;
      virtual int columnCount(const QModelIndex& parent = QModelIndex()) const;
      virtual QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      virtual Protos::Common::Entry getEntry(const QModelIndex& index) const;

      virtual bool isDir(const QModelIndex& index) const;
      virtual QString getPath(const QModelIndex& index, bool appendFilename = true) const;

      void refresh();

      QModelIndex searchChild(const QString name, const QModelIndex& parent = QModelIndex());

      bool isWaitingResult() const;

   signals:
      void loadingResultFinished();

   protected slots:
      virtual void resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries);
      virtual void result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries);
      virtual void resultTimeout();

   protected:
      virtual void browse(const Common::Hash& peerID, Node* node = 0);
      virtual void loadChildren(const QPersistentModelIndex &index);
      virtual QList<Node*> synchronize(BrowseModel::Node* node, const Protos::Common::Entries& entries);
      virtual QList<Node*> synchronizeRoot(const Protos::Common::Entries& entries);

      class Node
      {
      public:
         class NodeBreadthIterator
         {
         public:
            NodeBreadthIterator(Node* node);
            Node* next();

         private:
            void readChildren(Node* parentNode);

            QList<Node*> nextNodes;
         };

         Node();
         Node(const Protos::Common::Entry& entry, Node* parent);
         virtual ~Node();

         virtual Node* getParent();
         virtual int getNbChildren() const;
         virtual Node* getChild(int row) const;
         virtual void moveChild(int from, int to);
         virtual void insertChildren(const Protos::Common::Entries& entries);
         virtual void insertChild(const Protos::Common::Entry& entry, int pos);
         virtual bool hasUnloadedChildren() const;

         virtual int getRow() const;
         virtual QVariant getData(int column) const;
         virtual const Protos::Common::Entry& getEntry() const;
         virtual void setEntry(const Protos::Common::Entry& entry);

      protected:
         virtual Node* newNode(const Protos::Common::Entry& entry);
         virtual Node* newNode(const Protos::Common::Entry& entry, int pos);
         virtual void copySharedDirFromParent();

         Protos::Common::Entry entry;
         Node* parent;
         QList<Node*> children;
      };

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      const DirListModel& sharedDirsModel;
      Common::Hash peerID;

      QPersistentModelIndex currentBrowseIndex; // When we receive some entries after a browse query, they will be added as children to this index.
      QSharedPointer<RCC::IBrowseResult> browseResult;

      Node* root; // The corresponding index is null: QModelIndex().
   };

   bool operator>(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2);
   bool operator<(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2);
   bool operator==(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2);
   bool operator!=(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2);
}

#endif
