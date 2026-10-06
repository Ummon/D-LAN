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

#include <functional>

#include <QDialog>
#include <QMessageBox>
#include <QSharedPointer>
#include <QStringList>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class Utils
   {
   public:
      /**
        * Shows the given dialog as a modal one without running a local event loop, unlike 'QDialog::exec()'.
        * A widget can be deleted while one of its dialogs is open: its tab is removed when the connection to the
        * core is lost, the main window is deleted when the application is exited from the tray icon. A dialog
        * declared as a local variable and run by 'exec()' is then deleted a second time by its parent, and the
        * caller continues in a deleted object.
        * The dialog must have been allocated with 'new', it's deleted when it's closed or with its parent.
        * Its result is given by its signals, 'accepted()' or 'finished(..)' for instance.
        */
      static void showModal(QDialog* dialog)
      {
         dialog->setAttribute(Qt::WA_DeleteOnClose);
         dialog->setModal(true);
         dialog->show();
      }

      /**
        * As 'QMessageBox::information(..)' but without a local event loop, see 'showModal(..)'.
        */
      static void showInformation(QWidget* parent, const QString& title, const QString& text)
      {
         Utils::showModal(new QMessageBox(QMessageBox::Information, title, text, QMessageBox::Ok, parent));
      }

      static void askForDirectoriesOrFiles(
         QWidget* parent,
         QSharedPointer<RCC::ICoreConnection> coreConnection,
         const QString& title,
         const std::function<void(const QStringList&)>& selected
      );

      static void askForADirectoryToDownloadTo(
         QWidget* parent,
         QSharedPointer<RCC::ICoreConnection> coreConnection,
         const std::function<void(const QString&)>& selected
      );

      static QString emoticonsDirectoryPath();

      static void openLocations(const QStringList& paths, QWidget* parent = nullptr);
      static void openLocation(const QString& path, QWidget* parent = nullptr);
      static void openFile(const QString& path);
   };
}
