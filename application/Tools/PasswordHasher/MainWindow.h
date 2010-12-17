#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

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
      void savePassword();

   private:
      void setButtonText();
      QString checkPasswords() const;

   private:
      const QString CORE_SETTINGS_PATH;

      Ui::MainWindow *ui;
   };
}

#endif
