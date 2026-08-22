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

#include <Protos/gui_protocol.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/Containers/Tree.h>
#include <Common/RemoteCoreController/ILocalBrowseResult.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Settings/SharedEntryListModel.h>

namespace GUI
{
   class RemoteBrowseModel : public QAbstractItemModel
   {
      Q_OBJECT

      class Tree;

      enum Columns
      {
         NAME = 0,
         DATE_MODIFIED = 1,
         SIZE = 2
      };

   public:
      enum Filter {
         FILE = 1 << 0,
         DIR =  1 << 1
      };
      Q_DECLARE_FLAGS(Filters, Filter)

      RemoteBrowseModel(QSharedPointer<RCC::ICoreConnection> coreConnection);
      virtual ~RemoteBrowseModel();

      bool isLocal() const;

      virtual QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const override;
      virtual QModelIndex parent(const QModelIndex& child) const override;
      virtual int rowCount(const QModelIndex& parent = QModelIndex()) const override;
      virtual int columnCount(const QModelIndex& parent = QModelIndex()) const override;
      virtual QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;

      void setFilters(Filters filters);
      QString getPath(const QModelIndex& index, bool appendFilename = true) const;
      void getIndexFromPath(const QString& path);

   signals:
      void indexFromPath(const QModelIndex& index);

   private slots:
      void result(const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries);
      void resultTimeout();

   private:
      void browse(Tree* tree);
      void loadChildren(const QPersistentModelIndex &index);

      void exploreDirectories();
      QModelIndex indexFromTree(Tree* tree) const;

      class Tree : public Common::Tree<Protos::GUI::LocalBrowseResult::Entry, Tree>
      {
      public:
         Tree();
         Tree(const Protos::GUI::LocalBrowseResult::Entry& entry, Tree* parent);
         virtual ~Tree();

         void insertChildren(const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseResult::Entry>& entries);
         bool hasUnloadedChildren() const;
         QVariant data(int column) const;

         QString path() const;
      };

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      Filters filters;

      // When we receive some entries after a browse query, they will be added as children to this index.
      QPersistentModelIndex currentBrowseIndex;
      QSharedPointer<RCC::ILocalBrowseResult> localBrowseResult;

      // Used when we want to display a specific path with the method 'getIndexFromPath'.
      QList<QString> directoriesToExplore;
      Tree* currentTreeExploring;

      Tree* root; // The corresponding index is null: QModelIndex().
   };

   Q_DECLARE_OPERATORS_FOR_FLAGS(RemoteBrowseModel::Filters);
}
