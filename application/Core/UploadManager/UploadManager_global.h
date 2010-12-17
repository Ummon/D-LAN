/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef UPLOADMANAGER_GLOBAL_H
#define UPLOADMANAGER_GLOBAL_H

#include <QtCore/qglobal.h>

#if defined(UPLOADMANAGER_LIBRARY)
#  define UPLOADMANAGERSHARED_EXPORT Q_DECL_EXPORT
#else
#  define UPLOADMANAGERSHARED_EXPORT Q_DECL_IMPORT
#endif

#endif // UPLOADMANAGER_GLOBAL_H
