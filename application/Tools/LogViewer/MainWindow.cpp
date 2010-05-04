#include "MainWindow.h"
#include "ui_MainWindow.h"

#include <QFileDialog>
#include <Common/Constants.h>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    currentFile(0)
{
    ui->setupUi(this);
    connect(this->ui->actOpen, SIGNAL(activated()), this, SLOT(openDir()));

    this->currentDir.setSorting(QDir::Name);

    this->ui->tblLog->horizontalHeader()->setResizeMode(QHeaderView::ResizeToContents);
    this->ui->tblLog->verticalHeader()->setResizeMode(QHeaderView::Custom);
    this->ui->tblLog->verticalHeader()->setDefaultSectionSize(17);    
    this->ui->tblLog->setModel(&this->model);
}

MainWindow::~MainWindow()
{
    delete ui;
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

   this->currentFile = new QFile(this->currentDir.absolutePath() + '/' + file);
   if (currentFile->exists() && this->currentFile->open(QIODevice::ReadOnly))
   {
      this->model.setDataSource(this->currentFile);
   }
}

/**
  * - Read the directory content
  * - Open the last log file
  */
void MainWindow::setCurrentDir(const QString& dir)
{
   this->currentDir.setPath(dir);
   QStringList entries(this->currentDir.entryList());
   disconnect(this->ui->cmbFile, SIGNAL(currentIndexChanged(QString)));
   foreach (QString d, entries)
   {
      if (d.endsWith(".log"))
      {
         this->ui->cmbFile->addItem(d);
      }
   }
   connect(this->ui->cmbFile, SIGNAL(currentIndexChanged(QString)), this, SLOT(setCurrentFile(QString)));
   this->ui->cmbFile->setCurrentIndex(this->ui->cmbFile->count() - 1);
}

void MainWindow::closeCurrentFile()
{
   this->model.removeDataSource();
   if (this->currentFile)
   {
      delete this->currentFile;
   }
}
