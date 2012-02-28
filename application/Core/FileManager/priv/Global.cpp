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
  
#include <priv/Global.h>
using namespace FM;

#include <Common/Settings.h>

const QString& Global::getUnfinishedSuffix()
{
   static const QString suffix = SETTINGS.get<QString>("unfinished_suffix_term");
   return suffix;
}

bool Global::isFileUnfinished(const QString& filename)
{
   return filename.size() > Global::getUnfinishedSuffix().size() && filename.endsWith(Global::getUnfinishedSuffix());
}

QString Global::removeUnfinishedSuffix(const QString& filename)
{
   if (Global::isFileUnfinished(filename))
      return filename.left(filename.size() - Global::getUnfinishedSuffix().size());
   return filename;
}
