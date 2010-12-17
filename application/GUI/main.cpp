/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <QtGui/QApplication>

#include <Protos/gui_settings.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/Settings.h>
#include <Common/Constants.h>

#include <AybabtuGUI.h>

int main(int argc, char *argv[])
{
   SETTINGS.setFilename(Common::GUI_SETTINGS_FILENAME);
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   SETTINGS.load();
   SETTINGS.save(); // To automatically create the file if it doesn't exist.

   LM::Builder::setLogDirName("log_gui");

   GUI::AybabtuGUI gui(argc, argv);
   return gui.exec();
}
