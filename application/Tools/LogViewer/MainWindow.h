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
  
#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QDir>
#include <QLabel>
#include <QStringList>
#include <QFileSystemWatcher>

#include <TableLogModel.h>
#include <TooglableList/TooglableList.h>

namespace Ui { class MainWindow; }

class MainWindow : public QMainWindow
{
   Q_OBJECT
public:
   MainWindow(QWidget* parent = 0);
   ~MainWindow();

protected:
   void changeEvent(QEvent *e);

private slots:
   void openDir();
   void setCurrentFile(QString file);

   void filtersChange();
   void checkAll();

   void reloadAll();
   void newLogEntries(int n);

   void setWatchingPause(bool pause);
   void directoryChanged();

private:
   void setCurrentDir(const QString& dir);
   void readCurrentDir();
   void closeCurrentFile();
   void refreshFilters();
   void filterRow(int r);

   bool disableRefreshFilters;

   TooglableList* severities;
   TooglableList* modules;
   TooglableList* threads;

   QLabel* lblStatus;

   Ui::MainWindow* ui;

   TableLogModel model;

   QDir currentDir;
   QFile* currentFile;
   QFileSystemWatcher watcher;
};

#endif
