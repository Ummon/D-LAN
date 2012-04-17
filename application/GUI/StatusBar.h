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
  
#ifndef GUI_STATUSBAR_H
#define GUI_STATUSBAR_H

#include <QWidget>

#include <Protos/gui_protocol.pb.h>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace Ui {
   class StatusBar;
}

namespace GUI
{
   class StatusBar : public QWidget
   {
      Q_OBJECT

   public:
      explicit StatusBar(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = nullptr);
      ~StatusBar();

   signals:
      void showDockLog(bool);

   public slots:
      void dockLogVisibilityChanged(bool);

   private slots:
      void coreConnected();
      void coreDisconnected();
      void newState(const Protos::GUI::State& state);

      void showAbout();

   private:
      void setDownloadRate(qint64 rate);
      void setUploadRate(qint64 rate);
      void setTotalSharing(int nbPeer, qint64 amount);
      void updateCoreStatus(Protos::GUI::State_Stats_CacheStatus status = Protos::GUI::State_Stats_CacheStatus_UNKNOWN, int progress = 0);

      Ui::StatusBar* ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
   };
}

#endif
