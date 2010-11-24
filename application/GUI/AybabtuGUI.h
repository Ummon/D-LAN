#ifndef GUI_AYBABTUGUI_H
#define GUI_AYBABTUGUI_H

#include <QApplication>
#include <QMenu>
#include <QSystemTrayIcon>

#include <MainWindow.h>

namespace GUI
{
   class AybabtuGUI : public QApplication
   {
      Q_OBJECT
   public:
      AybabtuGUI(int argc, char *argv[]);

   private slots:
      void trayIconActivated(QSystemTrayIcon::ActivationReason reason);
      void mainWindowClosed();
      void showMainWindow();
      void exit();

   private:
      MainWindow* mainWindow;

      QSystemTrayIcon trayIcon;
      QMenu trayIconMenu;
   };
}

#endif
