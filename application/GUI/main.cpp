/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
#include <QTextCodec>
#include <QLocale>

#include <Protos/gui_settings.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/Settings.h>
#include <Common/Constants.h>

#include <D-LAN_GUI.h>

#if defined(DEBUG) && defined(ENABLE_NVWA)
   // For Common/debug_new.cpp.
   extern const char* new_progname;
#endif

/**
  * Arguments : [--lang <language>]
  *  --lang <language> : set the language and save it to the settings file. (ISO-63, two letters). Do not start the gui in this case.
  */
int main(int argc, char *argv[])
{
#if defined(DEBUG) && defined(ENABLE_NVWA)
   new_progname = argv[0];
#endif

   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

   QLocale locale;
   for (int i = 1; i < argc; i++)
   {
      const QString arg = QString::fromLatin1(argv[i]);
      if (arg == "--lang" && i < argc - 1)
         locale = QLocale(QString::fromLatin1(argv[++i]));
   }

   SETTINGS.setFilename(Common::Constants::GUI_SETTINGS_FILENAME);
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   SETTINGS.load();
   if (locale != QLocale::system())
      SETTINGS.set("language", locale);
   SETTINGS.save(); // To automatically create the file if it doesn't exist.

   LM::Builder::setLogDirName("log_gui");

   if (locale != QLocale::system())
      return 0;

   GUI::D_LAN_GUI gui(argc, argv);
   return gui.exec();
}
