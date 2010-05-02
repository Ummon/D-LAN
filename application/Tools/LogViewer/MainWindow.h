#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QDir>

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

private:
   void setCurrentDir(const QString& dir);
   void setCurrentFile(const QString& file);
   void closeCurrentFile();

   Ui::MainWindow *ui;

   QDir currentDir;
   QFile* currentFile;
};

#endif
