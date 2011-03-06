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
  
#include <QString>
#include <QTextCodec>

#include <Common/Global.h>

#include <CoreService.h>

#if defined(DEBUG) && defined(ENABLE_NVWA)
   // For Common/debug_new.cpp.
   extern const char* new_progname;
#endif

/**
  * Arguments : [-r <roaming data folder>] [-l <local data folder>]
  *  <roaming data folder> : Where settings are put.
  *  <local data folder> : Where logs, download queue, and files cache are put.
  */
int main(int argc, char* argv[])
{
#if defined(DEBUG) && defined(ENABLE_NVWA)
   new_progname = argv[0];
#endif

   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

   for (int i = 1; i < argc; i++)
   {
      const QString arg = QString::fromLatin1(argv[i]);
      if (arg == "-r" && i < argc - 1)
         Common::Global::setDataFolder(Common::Global::ROAMING, QString::fromLatin1(argv[i++]));
      else if (arg == "-l" && i < argc - 1)
         Common::Global::setDataFolder(Common::Global::LOCAL, QString::fromLatin1(argv[i++]));
   }

   CoreSpace::CoreService core(argc, argv);
   return core.exec();
}
