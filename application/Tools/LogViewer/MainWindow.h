#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QDir>

#include <TableLogModel.h>
#include <TooglableList.h>

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

private:
   void setCurrentDir(const QString& dir);
   void closeCurrentFile();
   void refreshFilters();

   TooglableList* severities;
   TooglableList* modules;
   TooglableList* threads;

   Ui::MainWindow* ui;

   TableLogModel model;

   QDir currentDir;
   QFile* currentFile;
};

#endif
