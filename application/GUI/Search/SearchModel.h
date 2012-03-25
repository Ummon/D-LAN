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
  
#ifndef GUI_SEARCHMODEL_H
#define GUI_SEARCHMODEL_H

#include <QAbstractItemModel>
#include <QTimer>
#include <QHash>

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/RemoteCoreController/ISearchResult.h>

#include <Browse/BrowseModel.h>
#include <PeerList/PeerListModel.h>
#include <Settings/DirListModel.h>

namespace GUI
{
   class SearchModel : public BrowseModel
   {
      class SearchTree;
      static const int NB_SIGNAL_PROGRESS; // The number of signal progress sent during a search.
      Q_OBJECT
   public:
      SearchModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel);
      ~SearchModel();

      Common::Hash getPeerID(const QModelIndex& index) const;

      void search(const QString& terms);

      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;

      int getNbFolders() const;
      int getNbFiles() const;

   signals:
      /**
        * 0 to 100;
        */
      void progress(int);

   protected:
      void loadChildren(const QPersistentModelIndex &index);

   protected slots:
      void result(const Protos::Common::FindResult& findResult);
      void sendNextProgress();
      void stopSearching();

   private:
      SearchTree* getRoot();
      int insertTree(const Protos::Common::FindResult_EntryLevel& entry, const Common::Hash& peerID, int currentIndex);
      bool setMaxLevel(int newLevel);

      const PeerListModel& peerListModel;

      QSharedPointer<RCC::ISearchResult> searchResult;

      int maxLevel;

      int nbFolders;
      int nbFiles;

      QTimer timerProgress;
      QTimer timerTimeout;

      int currentProgress;

      QHash<Common::Hash, SearchTree*> indexedFile;

      class SearchTree : public Tree
      {
      public:
         static QString entryPath(const Protos::Common::Entry& entry);

         SearchTree();
         SearchTree(const Protos::Common::Entry& entry, int level, const Common::Hash& peerID, const QString& peerNick, SearchTree* parent);
         SearchTree(const Protos::Common::Entry& entry, const Common::Hash& peerID,  SearchTree* parent);

         SearchTree* insertChild(const Protos::Common::FindResult::EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick);
         SearchTree* insertChild(int index, const Protos::Common::FindResult::EntryLevel& entry, const Common::Hash& peerID, const QString& peerNick);
         SearchTree* insertChild(SearchTree* node);

         int getLevel() const;
         Common::Hash getPeerID() const;
         QVariant data(int column) const;

         void copyFrom(const SearchTree* otherNode);
         bool isSameAs(const Protos::Common::Entry& otherEntry) const;

      protected:
         SearchTree* newTree(const Protos::Common::Entry& entry);

      private:
         int level;
         Common::Hash peerID;
         QString peerNick;
      };
   };
}

#endif
