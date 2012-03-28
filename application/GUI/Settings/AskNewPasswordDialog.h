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
  
#ifndef GUI_ASKNEWPASSWORDDIALOG_H
#define GUI_ASKNEWPASSWORDDIALOG_H

#include <QDialog>

#include <Common/Hash.h>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace Ui {
   class AskNewPasswordDialog;
}

namespace GUI
{
   class AskNewPasswordDialog : public QDialog
   {
      Q_OBJECT

   public:
      AskNewPasswordDialog(QSharedPointer<RCC::ICoreConnection> coreConnection, bool askOldPassword, QWidget* parent = 0);
      ~AskNewPasswordDialog();

   private slots:
      void ok();

   private:
      Ui::AskNewPasswordDialog* ui;
      QSharedPointer<RCC::ICoreConnection> coreConnection;
   };
}

#endif
