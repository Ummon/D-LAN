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

#include <QApplication>
#include <QTextCodec>
#include <QLocale>

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/Constants.h>
#include <Common/LogManager/Builder.h>

#include <D-LAN_GUI.h>

#if defined(DEBUG) && defined(ENABLE_NVWA)
   // For Common/debug_new.cpp.
   extern const char* new_progname;
#endif

Protos::GUI::Settings* createDefaultValuesSettings();

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
   SETTINGS.setSettingsMessage(createDefaultValuesSettings());
   SETTINGS.load();
   if (locale != QLocale::system())
      SETTINGS.set("language", locale);
   SETTINGS.save(); // To automatically create the file if it doesn't exist.

   // 'locale' has been set with "--lang".
   if (locale != QLocale::system())
      return 0;

   LM::Builder::setLogDirName("log_gui");

   LM::Builder::newLogger("Main")->log(QObject::tr("D-LAN GUI version %1").arg(Common::Global::getVersionFull()), LM::SV_END_USER);

   try
   {
      GUI::D_LAN_GUI gui(argc, argv);
      return GUI::D_LAN_GUI::exec();
   }
   catch (GUI::D_LAN_GUI::AbortException&)
   {
      return 1;
   }
}

Protos::GUI::Settings* createDefaultValuesSettings()
{
   auto settings = new Protos::GUI::Settings();

   settings->set_core_address("localhost");
   settings->set_core_port(59485);

   settings->set_max_chat_message_displayed(500);
   settings->set_max_log_message_displayed(500);
   settings->set_search_time(3000);
   settings->set_socket_timeout(7000);

   settings->set_main_window_width(1200);
   settings->set_main_window_height(620);
   settings->set_main_window_maximized(false);

   settings->set_download_view(Protos::GUI::Settings::TREE_VIEW);

   settings->set_peer_sort_type(Protos::GUI::Settings::BY_SHARING_AMOUNT);

   settings->set_lan_speed(104857600);

   settings->set_multiple_instance_allowed(false);

   settings->set_default_emoticon_theme("Default");

   settings->set_search_advanced_visible(false);
   settings->set_search_type(0);
   settings->set_search_min_size_value(0);
   settings->set_search_min_size_unit(Protos::GUI::Settings::MIB);
   settings->set_search_max_size_value(0);
   settings->set_search_max_size_unit(Protos::GUI::Settings::MIB);
   settings->set_search_local(false);

   settings->set_room_sort_type(Protos::GUI::Settings::BY_NAME);

   return settings;

}
