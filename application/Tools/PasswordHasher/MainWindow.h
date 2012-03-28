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
  
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

#include <Libs/MersenneTwister.h>

namespace Ui {
   class MainWindow;
}

namespace PasswordHasher
{
   class MainWindow : public QMainWindow
   {
      Q_OBJECT
   public:
      explicit MainWindow(QWidget *parent = 0);
      ~MainWindow();

   private slots:
      void computeHash();
      void savePasswordToCurrentUser();
      void savePasswordToSystemUser();

   private:
      void savePassword(const QString& directory);
      void setButtonText();
      QString checkPasswords() const;

   private:
      const QString CORE_SETTINGS_PATH_CURRENT_USER;
      const QString CORE_SETTINGS_PATH_SYSTEM_USER;

      Ui::MainWindow* ui;
      MTRand mtrand;
      quint64 salt;
   };
}

#endif
