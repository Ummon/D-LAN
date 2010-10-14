/****************************************************************************
** 
** Copyright (c) 2009 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
** 
** This file is part of a Qt Solutions component.
**
** Commercial Usage  
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Solutions Commercial License Agreement provided
** with the Software or, alternatively, in accordance with the terms
** contained in a written agreement between you and Nokia.
** 
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
** 
** In addition, as a special exception, Nokia gives you certain
** additional rights. These rights are described in the Nokia Qt LGPL
** Exception version 1.1, included in the file LGPL_EXCEPTION.txt in this
** package.
** 
** GNU General Public License Usage 
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
** 
** Please note Third Party Software included with Qt Solutions may impose
** additional restrictions and it is the user's responsibility to ensure
** that they have met the licensing requirements of the GPL, LGPL, or Qt
** Solutions Commercial license and the relevant license of the Third
** Party Software they are using.
** 
** If you are unsure which license is appropriate for your use, please
** contact Nokia at qt-info@nokia.com.
** 
****************************************************************************/

#include "qtunixserversocket.h"
#include <sys/types.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <string.h>
#include <unistd.h>

#ifndef SUN_LEN
#define SUN_LEN(ptr) ((size_t)(((struct sockaddr_un *) 0)->sun_path) \
                      +strlen ((ptr)->sun_path))
#endif

QtUnixServerSocket::QtUnixServerSocket(const QString &path, QObject *parent)
    : QTcpServer(parent)
{
    setPath(path);
}

QtUnixServerSocket::QtUnixServerSocket(QObject *parent)
    : QTcpServer(parent)
{
}

void QtUnixServerSocket::setPath(const QString &path)
{
    path_.clear();

    int sock = ::socket(PF_UNIX, SOCK_STREAM, 0);
    if (sock != -1) {
	struct sockaddr_un addr;
	::memset(&addr, 0, sizeof(struct sockaddr_un));
	addr.sun_family = AF_UNIX;
	::unlink(path.toLatin1().constData()); // ### This might need to be changed
	unsigned int pathlen = strlen(path.toLatin1().constData());
	if (pathlen > sizeof(addr.sun_path)) pathlen = sizeof(addr.sun_path);
	::memcpy(addr.sun_path, path.toLatin1().constData(), pathlen);
	if ((::bind(sock, (struct sockaddr *)&addr, SUN_LEN(&addr)) != -1) &&
	    (::listen(sock, 5) != -1)) {
	    setSocketDescriptor(sock);
            path_ = path;
        }
    }
}

void QtUnixServerSocket::close()
{
    QTcpServer::close();
    if (!path_.isEmpty()) {
        ::unlink(path_.toLatin1().constData());
        path_.clear();
    }
}
