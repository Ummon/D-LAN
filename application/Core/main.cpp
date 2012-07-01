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
  
#include <QString>
#include <QTextCodec>
#include <QTextStream>
#include <QLocale>

#include <Common/Global.h>
#include <Common/LogManager/Builder.h>

#include <CoreService.h>

#if defined(DEBUG) && defined(ENABLE_NVWA)
   // For Libs/debug_new.cpp.
   extern const char* new_progname;
   extern bool new_verbose_flag;
   extern FILE* new_output_fp;
#endif

/**
  * Arguments : [-r <roaming data directory>] [-l <local data directory>] [--reset-settings] [--lang <language>] [--version] [<arguments from QtService>]
  *  <roaming data directory> : Where settings are put.
  *  <local data directory> : Where logs, download queue, and files cache are put.
  *  --reset-settings : Remove all settings except "nick" and "peerID", other settings are set to their default values. Core exist directly after.
  *  --lang <language> : set the language and save it to the settings file. (ISO-639, two letters)
  *  --version : Print the version
  *  <arguments from QtService> : Type "D-LAN.Core.exe -h" to see them.
  */
int main(int argc, char* argv[])
{
#if defined(DEBUG) && defined(ENABLE_NVWA)
   new_progname = argv[0];
#endif

   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

   bool resetSettings = false;
   QLocale locale;
   for (int i = 1; i < argc; i++)
   {
      const QString arg = QString::fromLatin1(argv[i]);
      if (arg == "-r" && i < argc - 1)
         Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, QString::fromLatin1(argv[++i]));
      else if (arg == "-l" && i < argc - 1)
         Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, QString::fromLatin1(argv[++i]));
      else if (arg == "--lang" && i < argc - 1)
         locale = QLocale(QString::fromLatin1(argv[++i]));
      else if (arg == "--reset-settings")
         resetSettings = true;
      else if (arg == "--version")
      {
         QTextStream out(stdout);
         const QString versionTag = Common::Global::getVersionTag();
         out << Common::Global::getVersion() % (versionTag.isEmpty() ? QString() : " " % versionTag) << " " << Common::Global::getBuildTime().toString("yyyy-MM-dd_HH-mm") << endl;
         return 0;
      }
   }

   LM::Builder::setLogDirName("log_core");

   CoreSpace::CoreService core(resetSettings, locale, argc, argv);

   if (resetSettings || locale != QLocale::system())
      return 0;
   else
      return core.exec();
}
