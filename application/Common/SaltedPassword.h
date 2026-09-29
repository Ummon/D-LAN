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

#pragma once

#include <QString>

#include <Common/Hash.h>

namespace Common
{
   /**
     * A password hashed with a salt: 'hash' = 'Hasher::hashWithSalt(password, salt)'.
     * Stored in the settings as a string "<hash>$<salt>", where <hash> is in hexadecimal and <salt> in decimal.
     * A null hash means no password is defined, its string form is empty.
     */
   struct SaltedPassword
   {
      Hash hash;
      quint64 salt = 0;

      bool isNull() const { return this->hash.isNull(); }

      QString toStr() const;
      static SaltedPassword fromStr(const QString& str);
   };
}
