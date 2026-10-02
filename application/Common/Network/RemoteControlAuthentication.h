#pragma once

#include <QByteArray>
#include <QByteArrayView>
#include <QSslCertificate>

/**
  * The cryptography of the remote control authentication, see 'Protos.GUI.AskForAuthentication'.
  */
namespace Common::RemoteControlAuthentication
{
   // Version 1 is the protocol of the versions prior to 1.4.2, without any version number.
   constexpr quint32 PROTOCOL_VERSION = 2;

   constexpr int KEY_SIZE = 32;
   constexpr int KDF_SALT_SIZE = 16;
   constexpr int NONCE_SIZE = 32;

   // Argon2id parameters. Lower ones are refused: a malicious core could make a GUI derive a key that is cheap
   // to crack from its proof. Higher ones are refused too: a core could make a GUI hang.
   constexpr quint32 KDF_MEMORY = 64 * 1024; // [KiB].
   constexpr quint32 KDF_ITERATIONS = 3;
   constexpr quint32 MAX_KDF_MEMORY = 256 * 1024; // [KiB].
   constexpr quint32 MAX_KDF_ITERATIONS = 10;

   bool isValidKdf(QByteArrayView kdfSalt, quint32 memory, quint32 iterations);

   // Argon2id with one lane. Slow by design. Throws a QString if the derivation fails.
   QByteArray deriveKey(QByteArrayView saltedPassword, QByteArrayView kdfSalt, quint32 memory, quint32 iterations);

   // From the system's cryptographically secure generator.
   QByteArray randomBytes(int size);

   // SHA-256 of the DER certificate of the core, empty for a null certificate (local connection without TLS).
   QByteArray channelBinding(const QSslCertificate& certificate);

   QByteArray clientProof(QByteArrayView key, quint64 challenge, QByteArrayView nonce, QByteArrayView binding);
   QByteArray coreProof(QByteArrayView key, quint64 challenge, QByteArrayView nonce, QByteArrayView binding);

   // In constant time.
   bool equals(QByteArrayView a, QByteArrayView b);
}
