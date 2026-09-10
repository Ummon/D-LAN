/****************************************************************************
**
** Copyright (C) 2013 Digia Plc and/or its subsidiary(-ies).
** Contact: http://www.qt-project.org/legal
**
** This file is part of the Qt Solutions component.
**
** $QT_BEGIN_LICENSE:BSD$
** You may use this file under the terms of the BSD license as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of Digia Plc and its Subsidiary(-ies) nor the names
**     of its contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "qtunixsocket.h"
#include <QFile>
#include <QScopeGuard>
#include <QThread>
#include <cerrno>
#include <cstddef>
#include <fcntl.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <string.h>
#include <unistd.h>

QtUnixSocket::QtUnixSocket(QObject *parent)
    : QTcpSocket(parent)
{
}

bool QtUnixSocket::connectTo(const QString &path, QDeadlineTimer deadline)
{
    struct sockaddr_un addr{};
    const QByteArray encodedPath = QFile::encodeName(path);
    // Reserve space for the terminator and never silently truncate a pathname.
    if (encodedPath.size() >= static_cast<qsizetype>(sizeof(addr.sun_path))) {
        errno = ENAMETOOLONG;
        return false;
    }
    if (encodedPath.isEmpty() || encodedPath.contains('\0')) {
        errno = EINVAL;
        return false;
    }
    addr.sun_family = AF_UNIX;
    ::memcpy(addr.sun_path, encodedPath.constData(), encodedPath.size());
    const socklen_t addressLength = static_cast<socklen_t>(offsetof(sockaddr_un, sun_path) + encodedPath.size() + 1);

    while (!deadline.hasExpired()) {
        const int sock = ::socket(PF_UNIX, SOCK_STREAM, 0);
        if (sock == -1)
            return false;
        auto closeSocket = qScopeGuard([sock] { ::close(sock); });
        const int flags = ::fcntl(sock, F_GETFL);
        if (flags == -1 || ::fcntl(sock, F_SETFL, flags | O_NONBLOCK) == -1)
            return false;

        if (::connect(sock, reinterpret_cast<struct sockaddr *>(&addr), addressLength) == -1) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                // A full Linux Unix-socket backlog has not started a connection.
                // Retry with a fresh socket, without spinning or resetting the deadline.
                const qint64 remaining = deadline.remainingTime();
                if (remaining > 0)
                    QThread::msleep(static_cast<unsigned long>(qMin<qint64>(10, remaining)));
                continue;
            }
            if (errno == EINTR)
                continue;
            if (errno != EINPROGRESS)
                return false;

            pollfd fd{sock, POLLOUT, 0};
            int ready;
            do {
                if (deadline.hasExpired())
                    return false;
                ready = ::poll(&fd, 1, static_cast<int>(deadline.remainingTime()));
            } while (ready < 0 && errno == EINTR);
            if (ready <= 0 || (fd.revents & POLLNVAL))
                return false;
            int error = 0;
            socklen_t errorSize = sizeof(error);
            if (::getsockopt(sock, SOL_SOCKET, SO_ERROR, &error, &errorSize) == -1 || error != 0)
                return false;
        }

        if (!setSocketDescriptor(sock))
            return false;
        closeSocket.dismiss(); // QTcpSocket now owns the descriptor.
        return true;
    }
    return false;
}
