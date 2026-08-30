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

#pragma once

#include <QAbstractItemModel>
#include <QVariant>

#include <Protos/common.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Hash.h>
#include <Common/Containers/Tree.h>
#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/RemoteCoreController/IBrowseResult.h>

#include <Settings/SharedEntryListModel.h>

namespace GUI
{
   class BrowseModel : public QAbstractItemModel
   {
      enum Columns
      {
         NAME = 0,
         SIZE = 1
      };

   protected:
      class Tree;

   private:
      Q_OBJECT

   public:
      BrowseModel(
         QSharedPointer<RCC::ICoreConnection> coreConnection,
         const SharedEntryListModel& sharedEntryListModel,
         const Common::Hash& peerID,
         bool loadRoots = true
      );
      virtual ~BrowseModel();

      virtual QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const override;
      virtual QModelIndex parent(const QModelIndex& child) const override;
      virtual int rowCount(const QModelIndex& parent = QModelIndex()) const override;
      virtual int columnCount(const QModelIndex& parent = QModelIndex()) const override;
      virtual QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;

      virtual Protos::Common::Entry getEntry(const QModelIndex& index) const;

      virtual bool isDir(const QModelIndex& index) const;
      virtual QString getPath(const QModelIndex& index, bool appendFilename = true) const;

      void refresh();

      QModelIndex searchChild(const QString& name, const QModelIndex& parent = QModelIndex());

      bool isWaitingResult() const;

      int nbSharedDirs() const;

   signals:
      void loadingResultFinished();

   protected slots:
      virtual void resultRefresh(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries);
      virtual void result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries);
      virtual void resultTimeout();

   protected:
      virtual void browse(Tree* Tree = nullptr);
      virtual void loadChildren(const QPersistentModelIndex &index);
      virtual void synchronize(BrowseModel::Tree* Tree, const Protos::Common::Entries& entries);
      virtual void synchronizeRoot(const Protos::Common::Entries& entries);
      virtual void reset();

      class Tree : public Common::Tree<Protos::Common::Entry, Tree>
      {
      public:
         Tree();
         Tree(const Protos::Common::Entry& entry, Tree* parent);
         virtual ~Tree();

         virtual void insertChildren(const Protos::Common::Entries& entries);
         virtual void setItem(const Protos::Common::Entry& entry) override;
         virtual bool hasUnloadedChildren() const;
         virtual QVariant data(int column) const;

      protected:
         virtual void copySharedDirFromParent();
      };

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      const SharedEntryListModel& sharedEntryListModel;
      Common::Hash peerID;

      // When we receive some entries after a browse query, they will be added as children to this index.
      QPersistentModelIndex currentBrowseIndex;
      QSharedPointer<RCC::IBrowseResult> browseResult;

      Tree* root; // The corresponding index is null: QModelIndex().
   };

   bool operator>(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2);
   bool operator<(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2);
   /**
     * Returns true if the two entries carry the same information from the point of view of the browse view.
     * This is not an equality test, see 'Protos::Common::operator==' in <Common/ProtoHelper.h> for that.
     */
   bool sameDisplayedContent(const Protos::Common::Entry& e1, const Protos::Common::Entry& e2);
}
