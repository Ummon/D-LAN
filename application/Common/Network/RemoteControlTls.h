#pragma once

#include <QSslConfiguration>
#include <QSslCertificate>
#include <QString>

namespace Common::RemoteControlTls
{
   // All files live below Global::getDataFolder(ROAMING). Throws QString on
   // failure; callers must keep remote access closed if persistence fails.
   QSslConfiguration serverConfiguration();
   QString fingerprint(const QSslCertificate& certificate);
   QString pinPath(const QString& host, quint16 port);
   void checkPeer(const QString& host, quint16 port, const QSslCertificate& certificate);
   // Trust on first use, committed only after application authentication.
   void rememberPeer(const QString& host, quint16 port, const QSslCertificate& certificate);
}
