#include "MainWindow.h"
#include "ui_MainWindow.h"

#include <QFileDialog>
#include <QFileInfo>

#include <Common/Constants.h>
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

   this->ui->tblLog->horizontalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblLog->verticalHeader()->setResizeMode(QHeaderView::Custom);
   this->ui->tblLog->verticalHeader()->setDefaultSectionSize(17);
   this->ui->tblLog->setModel(&this->model);
   this->ui->tblLog->setItemDelegate(new TableLogItemDelegate(this));

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
}

MainWindow::~MainWindow()
{
    delete this->ui;
    this->closeCurrentFile();
}

void MainWindow::changeEvent(QEvent *e)
{
    QMainWindow::changeEvent(e);
    switch (e->type()) {
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
      Common::APPLICATION_FOLDER_PATH + '/' + Common::LOG_FOLDER_NAME
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

   for (int i = 0; i < this->model.rowCount(); i++)
      this->filterRow(i);
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

/**
  * - Read the directory content
  * - Open the last log file
  */
void MainWindow::setCurrentDir(const QString& dir)
{
   this->currentDir.setPath(dir);
   if (!this->currentDir.exists())
      return;

   this->readCurrentDir();
   this->ui->cmbFile->setCurrentIndex(this->ui->cmbFile->count() - 1);
}

void MainWindow::readCurrentDir()
{
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
   connect(this->ui->cmbFile, SIGNAL(currentIndexChanged(QString)), this, SLOT(setCurrentFile(QString)));
}

void MainWindow::closeCurrentFile()
{
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
