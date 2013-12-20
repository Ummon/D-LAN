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
  
#ifndef GUI_SEARCHDOCK_H
#define GUI_SEARCHDOCK_H

#include <QString>
#include <QDockWidget>
#include <QSharedPointer>

#include <Protos/common.pb.h>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace Ui {
   class SearchDock;
}

namespace GUI
{
   class SearchDock : public QDockWidget
   {
      Q_OBJECT
      static QString getCategoryText(Protos::Common::FindPattern_Category category);

   public:
      explicit SearchDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = 0);
      ~SearchDock();

      void setFocusToLineEdit();

   signals:
      void search(const Protos::Common::FindPattern&);

   protected:
      void changeEvent(QEvent* event);
      bool eventFilter(QObject* obj, QEvent* event);

   private slots:
      void coreConnected();
      void coreDisconnected(bool force);

      void search();

      void saveSettings();

   private:
      void loadSettings();

      Ui::SearchDock* ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
   };
}

#endif
