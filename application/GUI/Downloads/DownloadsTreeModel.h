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

#include <Common/Tree.h>

#include <Downloads/DownloadsModel.h>

namespace GUI
{
   class DownloadsTreeModel : public DownloadsModel
   {
      Q_OBJECT

   public:
      DownloadsTreeModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter);
      ~DownloadsTreeModel();

      QList<quint64> getDownloadIDs(const QModelIndex& index) const;

      bool isDownloadPaused(const QModelIndex& index) const;
      bool isFileLocationKnown(const QModelIndex& index) const;
      bool isFileComplete(const QModelIndex& index) const;

      QString getPath(const QModelIndex& index, bool appendFilename = true) const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      QModelIndex index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
      QModelIndex parent(const QModelIndex& index) const;
      Qt::DropActions supportedDropActions() const;
      Qt::ItemFlags flags(const QModelIndex& index) const;

   protected:
      bool dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex& parent);

   protected slots:
      void onNewState(const Protos::GUI::State& state);

   private:
      class Tree : public Common::Tree<Protos::GUI::State::Download>
      {
      public:
         Tree();
         Tree(const Protos::GUI::State::Download& download, Tree* parent);
         ~Tree();

         void setToDelete(bool toDelete = true);
         bool isToDelete() const;

         void setNbPausedFiles(int n) { this->nbPausedFiles = n; }
         int getNbPausedFiles() const { return this->nbPausedFiles; }

         void setNbErrorFiles(int n) { this->nbErrorFiles = n; }
         int getNbErrorFiles() const {return this->nbErrorFiles; }

      protected:
         virtual Common::Tree<Protos::GUI::State::Download>* newTree(const Protos::GUI::State::Download& download);

      private:
         bool toDelete;
         int nbPausedFiles;
         int nbErrorFiles;
      };

      Tree* insertDirectory(Tree* tree, const QString& dir, const QString& peerSourceNick);
      Tree* insert(Tree* tree, const Protos::GUI::State::Download& download);

      void updateDirectoriesEntryDeleted(Tree* file, const Protos::GUI::State::Download& oldDownload);
      void updateDirectoriesNewFile(Tree* file);
      void updateDirectoriesFileModified(Tree* file, const Protos::GUI::State::Download& oldDownload);
      void updateDirectories(Tree* file, quint64 fileSizeDelta, quint64 fileDownloadedBytesDelta, Protos::GUI::State_Download::Status oldStatus = Protos::GUI::State::Download::QUEUED);

      Tree* root;
   };

   bool operator>(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2);
   bool operator<(const Protos::GUI::State::Download& d1, const Protos::GUI::State::Download& d2);
}

#endif
