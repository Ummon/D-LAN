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

#include <QByteArray>
#include <QString>

#include <Common/Hash.h>

namespace Common
{
   /**
     * The remote control password, known by the core and by the GUI controlling it remotely:
     * 'key' = Argon2id('Hasher::hashWithSalt(password, salt)', 'kdfSalt', 'kdfMemory', 'kdfIterations').
     * See 'Protos.GUI.AskForAuthentication' and 'Common::RemoteControlAuthentication'.
     * Stored in the settings as a string "argon2id$<kdf memory>$<kdf iterations>$<kdf salt>$<salt>$<key>", where the
     * KDF salt and the key are in hexadecimal and the other values in decimal.
     * A password without key is null, its string form is empty.
     *
     * The versions prior to 1.4.2 only kept 'Hasher::hashWithSalt(password, salt)', as "<hash>$<salt>". Such a legacy
     * password has no key, only a 'legacyHash' from which the key is derived, see 'upgraded()'.
     */
   struct SaltedPassword
   {
      quint64 salt = 0;
      QByteArray kdfSalt;
      quint32 kdfMemory = 0; // [KiB].
      quint32 kdfIterations = 0;
      QByteArray key;

      Hash legacyHash;

      bool isNull() const { return this->key.isEmpty(); }
      bool isLegacy() const { return this->isNull() && !this->legacyHash.isNull(); }

      // A complete key with accepted KDF parameters.
      bool isValid() const;

      // Whether both keys are derived with the same salts and KDF parameters.
      bool sameDerivation(const SaltedPassword& other) const;

      QString toStr() const;
      static SaltedPassword fromStr(const QString& str);

      // The following functions derive a key, they are slow by design. They throw a QString on failure.

      // With new random salts and the default KDF parameters.
      static SaltedPassword create(const QString& password);

      // The key of 'saltedPassword' = 'Hasher::hashWithSalt(password, salt)' for the given KDF salt and parameters.
      static SaltedPassword derive(const Hash& saltedPassword, quint64 salt, const QByteArray& kdfSalt, quint32 kdfMemory, quint32 kdfIterations);

      // The key of a legacy password, derived with a new random KDF salt and the default KDF parameters.
      SaltedPassword upgraded() const;
   };
}
