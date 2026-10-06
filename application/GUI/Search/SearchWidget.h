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
#include <QCache>
#include <QTextDocument>
#include <QItemSelection>
#include <QProgressBar>

#include <Protos/common.pb.h>

#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/Hash.h>

#include <Search/SearchModel.h>
#include <Browse/EntriesWidget.h>
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
      QTextDocument& textDocument(const QStyleOptionViewItem& option) const;
      QString toHtmlText(const QString& text) const;
      QStringList currentTerms;

      // The documents of the names lately painted or measured, by name. See 'textDocument(..)'.
      mutable QCache<QString, QTextDocument> documents { 256 };
   };

   class SearchMenu : public DownloadMenu
   {
      Q_OBJECT
   public:
      SearchMenu(QSharedPointer<RCC::ICoreConnection> coreConnection, const SharedEntryListModel& sharedEntryListModel) :
         DownloadMenu(coreConnection, sharedEntryListModel) {}
      void show(const QPoint& globalPosition, bool browseVisible);
   signals:
      void browse();
   private:
      void onShowMenu(QMenu& menu) override;
      bool browseVisible = false;
   };

   class SearchWidget : public EntriesWidget
   {
      Q_OBJECT
   public:
      explicit SearchWidget(
         QSharedPointer<RCC::ICoreConnection> coreConnection,
         PeerListModel& peerListModel,
         const SharedEntryListModel& sharedEntryListModel,
         const Protos::Common::FindPattern& findPattern,
         bool local = false,
         QWidget* parent = nullptr
      );
      ~SearchWidget();

   signals:
      void browse(const Common::Hash&, const Protos::Common::Entry&);

   protected:
      void changeEvent(QEvent* event) override;
      Common::Hash entryPeerID(const QModelIndex& index) const override;
      bool hasOwnLocation(const QModelIndex& index) const override;

   private slots:
      void displayContextMenuDownload(const QPoint& point);
      void entryDoubleClicked(const QModelIndex& index);
      void browseCurrents();
      void progress(int value);
      void treeviewSelectionChanged(const QItemSelection& selected, const QItemSelection& deselected);
      void treeviewSectionResized(int logicalIndex, int oldSize, int newSize);

   private:
      bool atLeastOneRemotePeer(const QModelIndexList& indexes) const;

      Ui::SearchWidget* ui;
      SearchMenu downloadMenu;

      SearchModel searchModel;
      SearchDelegate searchDelegate;
   };
}
