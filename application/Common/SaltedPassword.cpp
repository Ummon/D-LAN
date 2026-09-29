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

#include <Common/SaltedPassword.h>
using namespace Common;

static const QChar SEPARATOR('$');

/**
  * @return "<hash>$<salt>" or an empty string if the hash is null.
  */
QString SaltedPassword::toStr() const
{
   if (this->isNull())
      return QString();

   return this->hash.toStr() + SEPARATOR + QString::number(this->salt);
}

/**
  * @return A null password if 'str' is empty or malformed.
  */
SaltedPassword SaltedPassword::fromStr(const QString& str)
{
   const qsizetype separatorPos = str.indexOf(SEPARATOR);
   if (separatorPos < 0)
      return SaltedPassword();

   const auto hash = Hash::fromStr(str.left(separatorPos));
   if (!hash)
      return SaltedPassword();

   bool ok = false;
   const quint64 salt = str.mid(separatorPos + 1).toULongLong(&ok);
   if (!ok)
      return SaltedPassword();

   return SaltedPassword { *hash, salt };
}
