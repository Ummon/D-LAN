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

#include <QDialog>
#include <QStyledItemDelegate>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <RemoteBrowseDialog/RemoteBrowseModel.h>
#include <RemoteBrowseDialog/RemoteBrowseQuickAccessModel.h>

namespace Ui {
   class RemoteBrowseDialog;
}

namespace GUI
{
   class RemoteBrowseDialogDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class RemoteBrowseDialog : public QDialog
   {
      Q_OBJECT
   public:
      enum Mode
      {
         FILE =            1 << 0,
         DIR =             1 << 1,
         SELECT_MULTIPLE = 1 << 2
      };
      Q_DECLARE_FLAGS(Modes, Mode)

      explicit RemoteBrowseDialog(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = nullptr);
      ~RemoteBrowseDialog();

      void setModes(Modes modes);
      QStringList getSelectedPaths() const;      

   private slots:
      void accept() override;
      void reject() override;

   private:
      Ui::RemoteBrowseDialog *ui;

      RemoteBrowseModel model;
      RemoteBrowseQuickAccessModel modelQuickAccess;
      RemoteBrowseDialogDelegate delegate;
   };

   Q_DECLARE_OPERATORS_FOR_FLAGS(RemoteBrowseDialog::Modes);
}
