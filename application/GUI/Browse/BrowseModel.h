/**
  * Aybabtu - A decentralized LAN file sharing software.
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

#include <Common/ProtoHelper.h>
#include <Common/Hash.h>

#include <CoreConnection/CoreConnection.h>

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
      virtual void result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries);
      virtual void resultTimeout();

   protected:
      virtual void browse(const Common::Hash& peerID, Node* node = 0);
      virtual void loadChildren(const QPersistentModelIndex &index);

      class Node
      {
      public:
         Node();
         Node(const Protos::Common::Entry& entry, Node* parent);
         virtual ~Node();

         virtual Node* getParent();
         virtual int getNbChildren() const;
         virtual Node* getChild(int row) const;
         virtual void insertChildren(const Protos::Common::Entries& entries);
         virtual bool hasUnloadedChildren();

         virtual int getRow() const;
         virtual QVariant getData(int column) const;
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
