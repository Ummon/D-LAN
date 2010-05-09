#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QDir>
#include <QLabel>
#include <QStringList>

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

   void filtersChange();
   void checkAll();

   void reloadAll();
   void newLogEntries(int n);

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
};

#endif
