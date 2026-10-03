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

#include <iostream>

#include <QString>
#include <QTextStream>
#include <QLocale>
#include <QRegularExpression>

#include <Common/Global.h>
#include <Common/LogManager/Builder.h>
#include <Common/LogManager/CrashHandler.h>

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
   out << "Usage:" << Qt::endl <<
      " " << appName << " [-i|-u|-s|-t|-v] [--yes] [-r <roaming data directory>] [-l <local data directory>] [--port <remote control port>] [--reset-settings] [--lang <language>] [--pass <password> | --rmpass] [--version]" << Qt::endl <<
      "  -i, -u, -s, -t and -v must be the first argument." << Qt::endl <<
      "  Without -i, -u, -s, -t or -v the Core runs as a regular application." << Qt::endl <<
      "  The service is a Windows service or a systemd unit on Linux, it requires administrator rights. It isn't available on macOS." << Qt::endl <<
      "  -i [account] [password] : Install the service after a confirmation, optionally using given account and password (the password is only used on Windows)." << Qt::endl <<
      "  -u : Stop then uninstall the service after a confirmation." << Qt::endl <<
      "  -s : Launch the installed service." << Qt::endl <<
      "  -t : Stop the service." << Qt::endl <<
      "  -v : Print service status information." << Qt::endl <<
      "  --yes : Do not ask for confirmation with -i or -u." << Qt::endl <<
      "  <roaming data directory> : Where settings are put." << Qt::endl <<
      "  <local data directory> : Where logs, download queue, and files cache are put." << Qt::endl <<
      "  --port <remote control port> : Listen to this port for remote control (GUI) instead of the one defined in the settings. It isn't saved in the settings." << Qt::endl <<
      "  --reset-settings : Remove all settings except \"nick\" and \"peerID\" and quit, other settings are set to their default values." << Qt::endl <<
      "  --lang <language> : set the language and save it to the settings file then quit. (ISO-639, two letters)" << Qt::endl <<
      "  --pass <password> : set a password then quit. The core can be remotely controlled." << Qt::endl <<
      "  --rmpass : remove the current password." << Qt::endl <<
      "  --version : Print the version" << Qt::endl;
}

/**
  * See 'printUsage(..)' for more information about arguments.
  */
int main(int argc, char* argv[])
try
{
#if defined(DEBUG) && defined(ENABLE_NVWA)
   new_progname = argv[0];
#endif

   // Look for "-h" or "--help".
   for (int i = 1; i < argc; i++)
   {
      const QString arg = QString::fromLatin1(argv[i]);
      if (arg == "-h" || arg == "--help")
      {
         printUsage(QString::fromLatin1(argv[0]).split(QRegularExpression("\\\\|/")).last());
         return 0;
      }
   }

   bool resetSettings = false;
   QString newPassword;
   bool resetPassword = false;
   quint16 remoteControlPort = 0; // 0 -> use the setting 'remote_control_port'.
   QLocale locale;

   for (int i = 1; i < argc; i++)
   {
      const QString arg = QString::fromLatin1(argv[i]);
      if (arg == "-r" && i < argc - 1)
         Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, QString::fromLatin1(argv[++i]));
      else if (arg == "-l" && i < argc - 1)
         Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, QString::fromLatin1(argv[++i]));
      else if (arg == "--port" && i < argc - 1)
      {
         bool ok = false;
         const uint port = QString::fromLatin1(argv[++i]).toUInt(&ok);
         if (ok && port > 0 && port <= 65535)
            remoteControlPort = static_cast<quint16>(port);
         else
            std::cerr << "Invalid remote control port: " << argv[i] << std::endl;
      }
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
         out << Common::Global::getVersion() % (versionTag.isEmpty() ? QString() : " " % versionTag) << " " << Common::Global::getBuildTime().toString("yyyy-MM-dd_HH-mm") << Qt::endl;
         return 0;
      }
   }

   LM::Builder::setLogDirName("log_core");

   // Must come after 'setLogDirName(..)': the crash reports are written next to the log files.
   LM::CrashHandler::install();

   CoreSpace::CoreService core(resetSettings, locale, remoteControlPort, argc, argv);

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
catch (const std::exception& e)
{
   std::cerr << "Fatal error, type: " << typeid(e).name() << ", what: " << e.what() << std::endl;
   return 1;
}
catch (...)
{
   std::cerr << "Unknown fatal error" << std::endl;
   return 2;
}
