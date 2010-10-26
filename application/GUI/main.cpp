#include <QtGui/QApplication>
#include <MainWindow.h>
using namespace GUI;

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>

int main(int argc, char *argv[])
{
   SETTINGS.setFilename("gui_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   SETTINGS.load();
   SETTINGS.save(); // To automatically create the file if it doesn't exist.

   QApplication a(argc, argv);
   MainWindow w;
   w.show();

   return a.exec();
}
