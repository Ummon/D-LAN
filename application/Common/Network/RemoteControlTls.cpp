#include <Common/Network/RemoteControlTls.h>

#include <memory>

#include <QCryptographicHash>
#include <QDateTime>
#include <QDir>
#include <QFile>
#include <QHostAddress>
#include <QLockFile>
#include <QSaveFile>
#include <QSslKey>
#include <QSslSocket>

#include <Common/Global.h>

#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/rand.h>
#include <openssl/rsa.h>
#include <openssl/x509v3.h>

namespace
{
   template<typename T, auto Free>
   using Handle = std::unique_ptr<T, decltype(Free)>;

   QString tlsDirectory()
   {
      QString path;
      try
      {
         path = QDir(Common::Global::getDataFolder(Common::Global::DataFolderType::ROAMING)).filePath("remote-control-tls");
      }
      catch (const Common::Global::UnableToGetFolder& e)
      {
         throw e.errorMessage;
      }
      if (!QDir().mkpath(path))
         throw QString("Unable to create TLS directory: %1").arg(path);
      return path;
   }

   QByteArray readFile(const QString& path)
   {
      QFile file(path);
      if (!file.open(QIODevice::ReadOnly) || file.size() > 64 * 1024)
         throw QString("Unable to read TLS file: %1").arg(path);
      const auto data = file.readAll();
      if (file.error() != QFileDevice::NoError)
         throw QString("Unable to read TLS file: %1").arg(path);
      return data;
   }

   void writeFile(const QString& path, const QByteArray& data)
   {
      QSaveFile file(path);
      // QSaveFile keeps the old identity intact if a write fails. Never fall
      // back to a non-atomic write, or make a private key world-readable.
      if (!file.open(QIODevice::WriteOnly) ||
          !file.setPermissions(QFileDevice::ReadOwner | QFileDevice::WriteOwner) ||
          file.write(data) != data.size() || !file.commit())
         throw QString("Unable to persist TLS file: %1").arg(path);
   }

   QByteArray generateIdentity()
   {
      Handle<EVP_PKEY_CTX, EVP_PKEY_CTX_free> context(EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, nullptr), EVP_PKEY_CTX_free);
      EVP_PKEY* rawKey = nullptr;
      if (!context || EVP_PKEY_keygen_init(context.get()) <= 0 ||
          EVP_PKEY_CTX_set_rsa_keygen_bits(context.get(), 3072) <= 0 ||
          EVP_PKEY_keygen(context.get(), &rawKey) <= 0)
         throw QString("Unable to generate the remote-control TLS key");
      Handle<EVP_PKEY, EVP_PKEY_free> key(rawKey, EVP_PKEY_free);
      Handle<X509, X509_free> cert(X509_new(), X509_free);
      unsigned char serial[16];
      if (!cert || RAND_bytes(serial, sizeof serial) != 1)
         throw QString("Unable to generate the remote-control TLS certificate");
      serial[0] &= 0x7f;
      serial[0] |= 1;
      Handle<BIGNUM, BN_free> number(BN_bin2bn(serial, sizeof serial, nullptr), BN_free);
      if (!number || !BN_to_ASN1_INTEGER(number.get(), X509_get_serialNumber(cert.get())) ||
          X509_set_version(cert.get(), 2) != 1 ||
          !X509_gmtime_adj(X509_getm_notBefore(cert.get()), -300) ||
          !X509_gmtime_adj(X509_getm_notAfter(cert.get()), 10L * 365 * 24 * 60 * 60) ||
          X509_set_pubkey(cert.get(), key.get()) != 1)
         throw QString("Unable to initialize the remote-control TLS certificate");

      X509_NAME* name = X509_get_subject_name(cert.get());
      // Qt's Schannel backend uses CN as a persisted Windows key-container
      // name. With no CN it imports an ephemeral key instead. Our identity is
      // the certificate pin; hostname verification is not used for pairing.
      if (!name || X509_NAME_add_entry_by_txt(name, "O", MBSTRING_ASC,
             reinterpret_cast<const unsigned char*>("D-LAN Core"), -1, -1, 0) != 1 ||
          X509_set_issuer_name(cert.get(), name) != 1)
         throw QString("Unable to name the remote-control TLS certificate");

      const auto addExtension = [&](int nid, const char* value) {
         Handle<X509_EXTENSION, X509_EXTENSION_free> extension(
            X509V3_EXT_conf_nid(nullptr, nullptr, nid, value), X509_EXTENSION_free);
         if (!extension || X509_add_ext(cert.get(), extension.get(), -1) != 1)
            throw QString("Unable to configure the remote-control TLS certificate");
      };
      addExtension(NID_basic_constraints, "critical,CA:FALSE");
      addExtension(NID_key_usage, "critical,digitalSignature,keyEncipherment");
      addExtension(NID_ext_key_usage, "serverAuth");
      if (X509_sign(cert.get(), key.get(), EVP_sha256()) <= 0)
         throw QString("Unable to sign the remote-control TLS certificate");

      Handle<BIO, BIO_free> pem(BIO_new(BIO_s_mem()), BIO_free);
      if (!pem || PEM_write_bio_PrivateKey(pem.get(), key.get(), nullptr, nullptr, 0, nullptr, nullptr) != 1 ||
          PEM_write_bio_X509(pem.get(), cert.get()) != 1)
         throw QString("Unable to encode the remote-control TLS identity");
      char* data = nullptr;
      const long size = BIO_get_mem_data(pem.get(), &data);
      return QByteArray(data, size);
   }

   void validateCertificate(const QSslCertificate& cert)
   {
      const auto now = QDateTime::currentDateTimeUtc();
      if (cert.isNull() || cert.isBlacklisted() || !cert.effectiveDate().isValid() ||
          !cert.expiryDate().isValid() || now < cert.effectiveDate() || now >= cert.expiryDate())
         throw QString("The remote-control TLS certificate is invalid or expired");
   }
}

QSslConfiguration Common::RemoteControlTls::serverConfiguration()
{
   if (!QSslSocket::supportsSsl())
      throw QString("No Qt TLS backend is available");
   const QString path = QDir(tlsDirectory()).filePath("core.pem");
   QLockFile lock(path + ".lock");
   if (!lock.tryLock(5000))
      throw QString("Unable to lock the TLS identity: %1").arg(path);
   if (!QFile::exists(path))
      writeFile(path, generateIdentity());
   const auto pem = readFile(path);
   const QSslCertificate cert(pem);
   const QSslKey key(pem, QSsl::Rsa);
   validateCertificate(cert);

   // Check the pair, rather than deferring corrupt/mismatched keys to every
   // incoming TLS handshake. Existing identities are never silently replaced.
   Handle<BIO, BIO_free> certBio(BIO_new_mem_buf(pem.constData(), pem.size()), BIO_free);
   Handle<BIO, BIO_free> keyBio(BIO_new_mem_buf(pem.constData(), pem.size()), BIO_free);
   Handle<X509, X509_free> nativeCert(certBio ? PEM_read_bio_X509(certBio.get(), nullptr, nullptr, nullptr) : nullptr, X509_free);
   Handle<EVP_PKEY, EVP_PKEY_free> nativeKey(keyBio ? PEM_read_bio_PrivateKey(keyBio.get(), nullptr, nullptr, nullptr) : nullptr, EVP_PKEY_free);
   if (key.isNull() || !nativeCert || !nativeKey || X509_check_private_key(nativeCert.get(), nativeKey.get()) != 1 ||
       X509_verify(nativeCert.get(), nativeKey.get()) != 1)
      throw QString("Invalid TLS certificate/key pair: %1").arg(path);

   QSslConfiguration configuration = QSslConfiguration::defaultConfiguration();
   configuration.setProtocol(QSsl::TlsV1_2OrLater);
   configuration.setLocalCertificate(cert);
   configuration.setPrivateKey(key);
   configuration.setPeerVerifyMode(QSslSocket::VerifyNone); // The GUI authenticates with its password inside TLS.
   return configuration;
}

QString Common::RemoteControlTls::fingerprint(const QSslCertificate& certificate)
{
   return QString::fromLatin1(certificate.digest(QCryptographicHash::Sha256).toHex(':'));
}

QString Common::RemoteControlTls::pinPath(const QString& host, quint16 port)
{
   QString normalized = host.trimmed();
   const QHostAddress address(normalized);
   if (!address.isNull())
      normalized = address.toString();
   else
   {
      normalized = normalized.toLower();
      if (normalized.endsWith('.'))
         normalized.chop(1);
   }
   const auto id = QCryptographicHash::hash((normalized + '\n' + QString::number(port)).toUtf8(), QCryptographicHash::Sha256).toHex();
   return QDir(tlsDirectory()).filePath("peer-" + QString::fromLatin1(id) + ".pem");
}

void Common::RemoteControlTls::checkPeer(const QString& host, quint16 port, const QSslCertificate& certificate)
{
   validateCertificate(certificate);
   const QString path = pinPath(host, port);
   if (QFile::exists(path))
   {
      const QSslCertificate known(readFile(path));
      if (known.isNull() || known.toDer() != certificate.toDer())
         throw QString("TLS certificate changed for %1:%2. Received SHA-256 %3; trusted certificate: %4")
            .arg(host).arg(port).arg(fingerprint(certificate), path);
   }
}

void Common::RemoteControlTls::rememberPeer(const QString& host, quint16 port, const QSslCertificate& certificate)
{
   const QString path = pinPath(host, port);
   QLockFile lock(path + ".lock");
   if (!lock.tryLock()) // Do not block the GUI while another process holds a pin lock.
      throw QString("Unable to lock the trusted TLS certificate: %1").arg(path);
   checkPeer(host, port, certificate); // Another GUI may have pinned it during authentication.
   if (!QFile::exists(path))
      writeFile(path, certificate.toPem());
}
