#include <QtGui/QApplication>

#include <Protos/gui_settings.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/Settings.h>

#include <AybabtuGUI.h>

int main(int argc, char *argv[])
{
   SETTINGS.setFilename("gui_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   SETTINGS.load();
   SETTINGS.save(); // To automatically create the file if it doesn't exist.

   LM::Builder::setLogDirName("log_gui");

   GUI::AybabtuGUI gui(argc, argv);
   return gui.exec();
}
