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
  
#ifndef GUI_DOWNLOADSTREEMODEL_H
#define GUI_DOWNLOADSTREEMODEL_H

#include <QDragEnterEvent>

#include <Downloads/DownloadsModel.h>

namespace GUI
{
   class DownloadsTreeModel : public DownloadsModel
   {
      Q_OBJECT

   public:
      DownloadsTreeModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter);
      ~DownloadsTreeModel();

      quint64 getDownloadID(const QModelIndex& index) const;

      bool isDownloadPaused(const QModelIndex& index) const;
      bool isFileLocationKnown(const QModelIndex& index) const;
      bool isFileComplete(const QModelIndex& index) const;

      QString getPath(const QModelIndex& index, bool appendFilename = true) const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
      QModelIndex parent(const QModelIndex& child) const;
      Qt::DropActions supportedDropActions() const;
      Qt::ItemFlags flags(const QModelIndex& index) const;

   protected:
      bool dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex& parent);

   protected slots:
      void onNewState(const Protos::GUI::State& state);

   private:
      class Node
      {
      public:
         Node();
         Node(const Protos::GUI::State::Download& download, Node* parent);
         ~Node();
         Node* getChild(int row) const;

      private:
         bool toDelete;
         Protos::GUI::State::Download download;
         Node* parent;
         QList<Node*> children;
      };

      Node* root;
   };

   bool operator>(const Protos::GUI::Download& d1, const Protos::GUI::Download& d2);
   bool operator<(const Protos::GUI::Download& d1, const Protos::GUI::Download& d2);
}

#endif
