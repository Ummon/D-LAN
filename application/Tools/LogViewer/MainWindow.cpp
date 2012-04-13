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
  
#include <MainWindow.h>
#include <ui_MainWindow.h>

#include <QFileDialog>
#include <QFileInfo>

#include <Common/LogManager/Constants.h>
#include <Common/Global.h>
#include <TableLogItemDelegate.h>

MainWindow::MainWindow(QWidget *parent) :
   QMainWindow(parent),
   disableRefreshFilters(false),
   ui(new Ui::MainWindow),
   currentFile(0)
{
   this->ui->setupUi(this);

   connect(this->ui->actOpen, SIGNAL(activated()), this, SLOT(openDir()));
   connect(this->ui->butFilterAll, SIGNAL(clicked()), this, SLOT(checkAll()));
   connect(this->ui->butRefresh, SIGNAL(clicked()), this, SLOT(reloadAll()));

   this->currentDir.setSorting(QDir::Name);
   this->ui->tblLog->setWordWrap(false);
   this->ui->tblLog->setModel(&this->model);
   this->ui->tblLog->setItemDelegate(new TableLogItemDelegate(this));

   this->ui->tblLog->horizontalHeader()->setResizeMode(QHeaderView::Interactive);
   this->ui->tblLog->horizontalHeader()->resizeSection(0, 140);
   this->ui->tblLog->horizontalHeader()->resizeSection(1, 50);
   this->ui->tblLog->horizontalHeader()->resizeSection(2, 140);
   this->ui->tblLog->horizontalHeader()->resizeSection(3, 50);
   this->ui->tblLog->horizontalHeader()->resizeSection(4, 180);
   this->ui->tblLog->horizontalHeader()->resizeSection(5, 1200);
   this->ui->tblLog->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblLog->verticalHeader()->setDefaultSectionSize(17);
   this->ui->tblLog->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);

   this->severities = new TooglableList(this);
   this->modules = new TooglableList(this);
   this->threads = new TooglableList(this);
   connect(this->severities, SIGNAL(stateChanged()), this, SLOT(filtersChange()));
   connect(this->modules, SIGNAL(stateChanged()), this, SLOT(filtersChange()));
   connect(this->threads, SIGNAL(stateChanged()), this, SLOT(filtersChange()));

   this->ui->laySeverity->addWidget(this->severities);
   this->ui->layModule->addWidget(this->modules);
   this->ui->layThread->addWidget(this->threads);

   this->lblStatus = new QLabel(this->ui->statusBar);
   this->ui->statusBar->addWidget(this->lblStatus);

   connect(this->ui->butPause, SIGNAL(toggled(bool)), this, SLOT(setWatchingPause(bool)));
   this->setWatchingPause(false);
}

MainWindow::~MainWindow()
{
    delete this->ui;
    this->closeCurrentFile();
}

void MainWindow::changeEvent(QEvent *e)
{
    QMainWindow::changeEvent(e);
    switch (e->type())
    {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

/**
  * Ask the user to choose a directory. Trigged by the menu.
  */
void MainWindow::openDir()
{
   QString dir = QFileDialog::getExistingDirectory(
      this,
      "Choose a directory",
      Common::Global::getDataFolder(Common::Global::LOCAL, false) + '/' + LM::DEFAULT_LOG_FOLDER_NAME
   );

   if (!dir.isNull())
      this->setCurrentDir(dir);
}

/**
  * Open a file and set it as the current listened file.
  */
void MainWindow::setCurrentFile(QString file)
{
   this->closeCurrentFile();
   disconnect(&this->model, SIGNAL(newLogEntries(int)), 0, 0);

   this->currentFile = new QFile(this->currentDir.absolutePath() + '/' + file);
   if (currentFile->exists() && this->currentFile->open(QIODevice::ReadOnly))
   {
      this->model.setDataSource(this->currentFile);
      this->refreshFilters();

      // New messages can be logged after the file is loaded, thus some new severities/modules/threads can appear.
      connect(&this->model, SIGNAL(newSeverity(QString)), this->severities, SLOT(addItem(const QString&)));
      connect(&this->model, SIGNAL(newModule(QString)), this->modules, SLOT(addItem(const QString&)));
      connect(&this->model, SIGNAL(newThread(QString)), this->threads, SLOT(addItem(const QString&)));

      this->ui->tblLog->scrollToBottom();
      connect(&this->model, SIGNAL(newLogEntries(int)), this, SLOT(newLogEntries(int)));
   }
}

/**
  * Called when the filter is modified. It will refresh the entry list.
  */
void MainWindow::filtersChange()
{
   if (this->disableRefreshFilters)
      return;

   // TODO: find a better way to avoid slowing down.
   this->ui->tblLog->verticalHeader()->setResizeMode(QHeaderView::Custom);
   for (int i = 0; i < this->model.rowCount(); i++)
      this->filterRow(i);
   this->ui->tblLog->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
}

/**
  * Check all the filtered terms.
  */
void MainWindow::checkAll()
{
   this->disableRefreshFilters = true;
   this->severities->checkAll();
   this->modules->checkAll();
   this->threads->checkAll();
   this->disableRefreshFilters = false;
   this->filtersChange();
}

/**
  * Reread the current directory and reload the current file.
  */
void MainWindow::reloadAll()
{
   if (!this->currentFile || !this->currentDir.exists())
      return;

   this->readCurrentDir();

   this->ui->cmbFile->setCurrentIndex(this->ui->cmbFile->findText(QFileInfo(this->currentFile->fileName()).fileName()));
   this->currentFile->reset();
   this->model.setDataSource(this->currentFile);

   this->refreshFilters();
}

void MainWindow::newLogEntries(int n)
{
   for (int i = this->model.rowCount() - n; i < this->model.rowCount(); i++)
      this->filterRow(i);
   this->ui->tblLog->scrollToBottom();
}

void MainWindow::setWatchingPause(bool pause)
{
   if (pause)
      disconnect(&this->watcher, SIGNAL(directoryChanged(QString)), this, SLOT(directoryChanged()));
   else
      connect(&this->watcher, SIGNAL(directoryChanged(QString)), this, SLOT(directoryChanged()));

   this->model.setWatchingPause(pause);
}

void MainWindow::directoryChanged()
{
   if (!this->currentFile || !this->currentDir.exists())
      return;

   this->readCurrentDir();
}

/**
  * - Read the directory content
  * - Open the last log file
  */
void MainWindow::setCurrentDir(const QString& dir)
{
   this->watcher.removePath(this->currentDir.absolutePath());

   this->currentDir.setPath(dir);
   if (!this->currentDir.exists())
      return;

   this->watcher.addPath(this->currentDir.absolutePath());

   this->readCurrentDir();
}

/**
  * Read or refresh the current directory set with 'setCurrentDirectory'.
  * The newest file will become the current file and be automatically loaded.
  */
void MainWindow::readCurrentDir()
{
   this->currentDir.refresh();
   QStringList entries(this->currentDir.entryList());
   disconnect(this->ui->cmbFile, SIGNAL(currentIndexChanged(QString)), 0, 0);
   this->ui->cmbFile->clear();
   this->lblStatus->setText(this->currentDir.absolutePath());
   foreach (QString d, entries)
   {
      if (d.endsWith(".log"))
      {
         this->ui->cmbFile->addItem(d);
      }
   }
   this->ui->cmbFile->setCurrentIndex(this->ui->cmbFile->count() - 1);

   if (this-ui->cmbFile->count() > 0)
      this->setCurrentFile(this->ui->cmbFile->itemText(this->ui->cmbFile->count() - 1));

   connect(this->ui->cmbFile, SIGNAL(currentIndexChanged(QString)), this, SLOT(setCurrentFile(QString)));
}

void MainWindow::closeCurrentFile()
{
   disconnect(&this->model, SIGNAL(newSeverity(QString)), this->severities, SLOT(addItem(const QString&)));
   disconnect(&this->model, SIGNAL(newModule(QString)), this->modules, SLOT(addItem(const QString&)));
   disconnect(&this->model, SIGNAL(newThread(QString)), this->threads, SLOT(addItem(const QString&)));
   this->model.removeDataSource();
   if (this->currentFile)
   {
      delete this->currentFile;
      this->currentFile = 0;
   }
}

/**
  * Ask the log model wich are the known severities, modules and thread. Then refresh the
  * widgets 'TooglableList'.
  */
void MainWindow::refreshFilters()
{
   this->severities->setList(this->model.getSeverities());
   this->modules->setList(this->model.getModules());
   this->threads->setList(this->model.getThreads());
}

/**
  * Hide or show the given row depending the current filters.
  */
void MainWindow::filterRow(int r)
{
   if (this->model.isFiltered(r, this->severities->getList(), this->modules->getList(), this->threads->getList()))
      this->ui->tblLog->hideRow(r);
   else
      this->ui->tblLog->showRow(r);
}
