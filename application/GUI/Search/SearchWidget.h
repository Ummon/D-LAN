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

#include <QWidget>
#include <QString>
#include <QStyledItemDelegate>
#include <QPainter>
#include <QItemSelection>
#include <QProgressBar>

#include <Protos/common.pb.h>

#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/Hash.h>

#include <Search/SearchModel.h>
#include <Settings/SharedEntryListModel.h>
#include <DownloadMenu.h>

namespace Ui {
   class SearchWidget;
}

namespace GUI
{
   class SearchDelegate : public QStyledItemDelegate
   {
      static const QString MARKUP_FIRST_PART;
      static const QString MARKUP_SECOND_PART;

   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
      void setTerms(const QString& terms);

   private:
      QString toHtmlText(const QString& text) const;
      QStringList currentTerms;
      QProgressBar model;
   };

   class SearchMenu : public DownloadMenu
   {
      Q_OBJECT
   public:
      SearchMenu(const SharedEntryListModel& sharedEntryListModel) : DownloadMenu(sharedDirsModel) {}
   signals:
      void browse();
   private:
      virtual void onShowMenu(QMenu& menu);
   };

   class SearchWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit SearchWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const SharedEntryListModel& sharedEntryListModel, const Protos::Common::FindPattern& findPattern, bool local = false, QWidget* parent = nullptr);
      ~SearchWidget();

   signals:
      void browse(const Common::Hash&, const Protos::Common::Entry&);

   protected:
      void changeEvent(QEvent* event);
      void keyPressEvent(QKeyEvent* event);

   private slots:
      void displayContextMenuDownload(const QPoint& point);
      void entryDoubleClicked(const QModelIndex& index);

      void download();
      void downloadTo();
      void downloadTo(const QString& path, const Common::Hash& sharedDirID = Common::Hash());

      void openLocation();
      void browseCurrents();
      void progress(int value);
      void treeviewSelectionChanged(const QItemSelection& selected, const QItemSelection& deselected);
      void treeviewSectionResized(int logicalIndex, int oldSize, int newSize);

   private:
      bool atLeastOneRemotePeer(const QModelIndexList& indexes) const;

      void openFile(const QModelIndex& index) const;

      Ui::SearchWidget* ui;
      SearchMenu downloadMenu;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      SearchModel searchModel;
      SearchDelegate searchDelegate;
   };
}
