#include <Common/Network/RemoteControlAuthentication.h>

#include <memory>

#include <QCryptographicHash>
#include <QList>
#include <QMessageAuthenticationCode>
#include <QRandomGenerator>
#include <QtEndian>

#include <openssl/core_names.h>
#include <openssl/crypto.h>
#include <openssl/kdf.h>
#include <openssl/params.h>

namespace
{
   template<typename T, auto Free>
   using Handle = std::unique_ptr<T, decltype(Free)>;

   // HMAC-SHA256(key, label + '\0' + challenge (big endian) + nonce + binding).
   QByteArray proof(QByteArrayView label, QByteArrayView key, quint64 challenge, QByteArrayView nonce, QByteArrayView binding)
   {
      char challengeBytes[sizeof(challenge)];
      qToBigEndian(challenge, challengeBytes);

      QByteArray message;
      message.append(label).append('\0').append(challengeBytes, sizeof(challengeBytes)).append(nonce).append(binding);
      return QMessageAuthenticationCode::hash(message, key, QCryptographicHash::Sha256);
   }
}

bool Common::RemoteControlAuthentication::isValidKdf(QByteArrayView kdfSalt, quint32 memory, quint32 iterations)
{
   return
      kdfSalt.size() == KDF_SALT_SIZE &&
      memory >= KDF_MEMORY && memory <= MAX_KDF_MEMORY &&
      iterations >= KDF_ITERATIONS && iterations <= MAX_KDF_ITERATIONS;
}

QByteArray Common::RemoteControlAuthentication::deriveKey(QByteArrayView saltedPassword, QByteArrayView kdfSalt, quint32 memory, quint32 iterations)
{
   // Argon2 has been added to OpenSSL 3.2.
   Handle<EVP_KDF, EVP_KDF_free> kdf(EVP_KDF_fetch(nullptr, "ARGON2ID", nullptr), EVP_KDF_free);
   Handle<EVP_KDF_CTX, EVP_KDF_CTX_free> context(kdf ? EVP_KDF_CTX_new(kdf.get()) : nullptr, EVP_KDF_CTX_free);
   if (!context)
      throw QString("Argon2id is unavailable, OpenSSL 3.2 or later is required");

   quint32 lanes = 1;
   quint32 threads = 1;
   const OSSL_PARAM params[] {
      OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_PASSWORD, const_cast<char*>(saltedPassword.data()), saltedPassword.size()),
      OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SALT, const_cast<char*>(kdfSalt.data()), kdfSalt.size()),
      OSSL_PARAM_construct_uint32(OSSL_KDF_PARAM_ITER, &iterations),
      OSSL_PARAM_construct_uint32(OSSL_KDF_PARAM_ARGON2_MEMCOST, &memory),
      OSSL_PARAM_construct_uint32(OSSL_KDF_PARAM_ARGON2_LANES, &lanes),
      OSSL_PARAM_construct_uint32(OSSL_KDF_PARAM_THREADS, &threads),
      OSSL_PARAM_construct_end()
   };

   QByteArray key(KEY_SIZE, Qt::Uninitialized);
   if (EVP_KDF_derive(context.get(), reinterpret_cast<unsigned char*>(key.data()), key.size(), params) != 1)
      throw QString("Unable to derive the remote control key with Argon2id");
   return key;
}

QByteArray Common::RemoteControlAuthentication::randomBytes(int size)
{
   QList<quint32> words((size + 3) / 4);
   QRandomGenerator::system()->generate(words.begin(), words.end());
   return QByteArray(reinterpret_cast<const char*>(words.constData()), size);
}

QByteArray Common::RemoteControlAuthentication::channelBinding(const QSslCertificate& certificate)
{
   return certificate.isNull() ? QByteArray() : certificate.digest(QCryptographicHash::Sha256);
}

QByteArray Common::RemoteControlAuthentication::clientProof(QByteArrayView key, quint64 challenge, QByteArrayView nonce, QByteArrayView binding)
{
   return proof("D-LAN remote control GUI", key, challenge, nonce, binding);
}

QByteArray Common::RemoteControlAuthentication::coreProof(QByteArrayView key, quint64 challenge, QByteArrayView nonce, QByteArrayView binding)
{
   return proof("D-LAN remote control core", key, challenge, nonce, binding);
}

bool Common::RemoteControlAuthentication::equals(QByteArrayView a, QByteArrayView b)
{
   return a.size() == b.size() && CRYPTO_memcmp(a.data(), b.data(), a.size()) == 0;
}
