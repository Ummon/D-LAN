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

#include <QRegularExpression>
#include <QStringList>

#include <Common/Network/RemoteControlAuthentication.h>

namespace RCA = Common::RemoteControlAuthentication;

static const QChar SEPARATOR('$');
static const QString ALGORITHM("argon2id");

/**
  * @return An empty array unless 'str' is exactly 'size' bytes in hexadecimal ('QByteArray::fromHex(..)' skips invalid characters).
  */
static QByteArray fromHex(const QString& str, int size)
{
   static const QRegularExpression HEXADECIMAL("^[0-9a-fA-F]*$");
   if (str.size() != 2 * size || !HEXADECIMAL.match(str).hasMatch())
      return QByteArray();
   return QByteArray::fromHex(str.toLatin1());
}

bool SaltedPassword::isValid() const
{
   return this->key.size() == RCA::KEY_SIZE && RCA::isValidKdf(this->kdfSalt, this->kdfMemory, this->kdfIterations);
}

bool SaltedPassword::sameDerivation(const SaltedPassword& other) const
{
   return
      this->salt == other.salt && this->kdfSalt == other.kdfSalt &&
      this->kdfMemory == other.kdfMemory && this->kdfIterations == other.kdfIterations;
}

/**
  * @return An empty string if the password is null and not legacy.
  */
QString SaltedPassword::toStr() const
{
   if (this->isNull())
      return this->legacyHash.isNull() ? QString() : this->legacyHash.toStr() + SEPARATOR + QString::number(this->salt);

   return QStringList {
      ALGORITHM,
      QString::number(this->kdfMemory),
      QString::number(this->kdfIterations),
      QString::fromLatin1(this->kdfSalt.toHex()),
      QString::number(this->salt),
      QString::fromLatin1(this->key.toHex())
   }.join(SEPARATOR);
}

/**
  * @return A null password if 'str' is empty or malformed.
  */
SaltedPassword SaltedPassword::fromStr(const QString& str)
{
   const QStringList parts = str.split(SEPARATOR);
   bool ok = false;

   if (parts.size() == 2)
   {
      const auto hash = Hash::fromStr(parts[0]);
      const quint64 salt = parts[1].toULongLong(&ok);
      if (!hash || !ok)
         return SaltedPassword();

      SaltedPassword password;
      password.salt = salt;
      password.legacyHash = *hash;
      return password;
   }

   if (parts.size() != 6 || parts[0] != ALGORITHM)
      return SaltedPassword();

   SaltedPassword password;
   bool memoryOk = false, iterationsOk = false;
   password.kdfMemory = parts[1].toUInt(&memoryOk);
   password.kdfIterations = parts[2].toUInt(&iterationsOk);
   password.kdfSalt = fromHex(parts[3], RCA::KDF_SALT_SIZE);
   password.salt = parts[4].toULongLong(&ok);
   password.key = fromHex(parts[5], RCA::KEY_SIZE);

   if (!memoryOk || !iterationsOk || !ok || !password.isValid())
      return SaltedPassword();

   return password;
}

SaltedPassword SaltedPassword::create(const QString& password)
{
   quint64 salt = 0;
   const Hash saltedPassword = Hasher::hashWithRandomSalt(password, salt);
   return SaltedPassword::derive(saltedPassword, salt, RCA::randomBytes(RCA::KDF_SALT_SIZE), RCA::KDF_MEMORY, RCA::KDF_ITERATIONS);
}

SaltedPassword SaltedPassword::derive(const Hash& saltedPassword, quint64 salt, const QByteArray& kdfSalt, quint32 kdfMemory, quint32 kdfIterations)
{
   SaltedPassword password;
   password.salt = salt;
   password.kdfSalt = kdfSalt;
   password.kdfMemory = kdfMemory;
   password.kdfIterations = kdfIterations;
   password.key = RCA::deriveKey(QByteArrayView(saltedPassword.getData(), Hash::HASH_SIZE), kdfSalt, kdfMemory, kdfIterations);
   return password;
}

SaltedPassword SaltedPassword::upgraded() const
{
   return SaltedPassword::derive(this->legacyHash, this->salt, RCA::randomBytes(RCA::KDF_SALT_SIZE), RCA::KDF_MEMORY, RCA::KDF_ITERATIONS);
}
