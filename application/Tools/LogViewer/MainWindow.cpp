#include "MainWindow.h"
#include "ui_MainWindow.h"

#include <QFileDialog>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    currentFile(0)
{
    ui->setupUi(this);
    connect(this->ui->actOpen, SIGNAL(activated()), this, SLOT(openDir()));
    this->currentDir.setSorting(QDir::Name);
}

MainWindow::~MainWindow()
{
    delete ui;
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
   QString dir = QFileDialog::getExistingDirectory(this, "Choose a directory");
   if (!dir.isNull())
      this->setCurrentDir(dir);
}

/**
  * - Read the directory content
  * - Open the last log file
  */
void MainWindow::setCurrentDir(const QString& dir)
{
   this->currentDir.setPath(dir);
   QStringList entries(this->currentDir.entryList());
   foreach (QString d, entries)
   {
      if (d.endsWith(".log"))
      {
         this->ui->cmbFile->addItem(d);
      }
   }
   this->setCurrentFile(entries.last());
}

/**
  * Open a file, set it as the current listened file.
  */
void MainWindow::setCurrentFile(const QString& file)
{
   this->closeCurrentFile();

   this->currentFile = new QFile(file, QIODevice::ReadOnly);
}

void MainWindow::closeCurrentFile()
{
   if (this->currentFile)
   {
      delete this->currentFile;
   }
}
