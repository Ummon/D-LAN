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

void printUsage(QString appName)
{
   QTextStream out(stdout);
   out << "Usage:" << endl <<
          " " << appName << " [-r <roaming data directory>] [-l <local data directory>] [--reset-settings] [--lang <language>] [--pass <password> | --rmpass] [--version] [-i|-u|-e|-s|-v]" << endl <<
          "  -i [account] [password] : Install the service, optionally using given account and password" << endl <<
          "  -u : Uninstall the service." << endl <<
          "  -e : Run as a regular application. Otherwise try to launch the installed service." << endl <<
          "  -t : Stop the service." << endl <<
          "  -v : Print service status information." << endl <<
          "  <roaming data directory> : Where settings are put." << endl <<
          "  <local data directory> : Where logs, download queue, and files cache are put." << endl <<
          "  --reset-settings : Remove all settings except \"nick\" and \"peerID\" and quit, other settings are set to their default values." << endl <<
          "  --lang <language> : set the language and save it to the settings file then quit. (ISO-639, two letters)" << endl <<
          "  --pass <password> : set a password then quit. The core can be remotely controlled." << endl <<
          "  --rmpass : remove the current password." << endl <<
          "  --version : Print the version" << endl;
}

/**
  * See 'printUsage(..)' for more information about arguments.
  */
int main(int argc, char* argv[])
{
#if defined(DEBUG) && defined(ENABLE_NVWA)
   new_progname = argv[0];
#endif

   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

   // Look for "-h" or "--help".
   for (int i = 1; i < argc; i++)
   {
      const QString arg = QString::fromLatin1(argv[i]);
      if (arg == "-h" || arg == "--help")
      {
         printUsage(QString::fromLatin1(argv[0]).split(QRegExp("\\\\|/")).last());
         return 0;
      }
   }

   bool resetSettings = false;
   QString newPassword;
   bool resetPassword = false;
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
      else if (arg == "--pass" && i < argc - 1)
         newPassword = QString::fromLatin1(argv[++i]);
      else if (arg == "--rmpass")
         resetPassword = true;
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

   if (!newPassword.isEmpty())
   {
      core.changePassword(newPassword);
      return 0;
   }

   if (resetPassword)
   {
      core.removePassword();
      return 0;
   }

   if (resetSettings || locale != QLocale::system())
      return 0;
   else
      return core.exec();
}
