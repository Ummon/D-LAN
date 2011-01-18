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
  
#ifndef GUI_WIDGETSEARCH_H
#define GUI_WIDGETSEARCH_H

#include <QWidget>
#include <QString>
#include <QStyledItemDelegate>
#include <QPainter>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Search/SearchModel.h>
#include <Settings/DirListModel.h>

namespace Ui {
   class WidgetSearch;
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
   };

   class WidgetSearch : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetSearch(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const QString& terms, QWidget *parent = 0);
      ~WidgetSearch();

   private slots:
      void displayContextMenuPeers(const QPoint& point);
      void download();
      void progress(int value);

   private:
      Ui::WidgetSearch *ui;
      QSharedPointer<RCC::ICoreConnection> coreConnection;

      SearchModel searchModel;
      SearchDelegate searchDelegate;
   };
}

#endif
