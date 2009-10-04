#ifndef LOGMANAGERGLOBAL_H
#define LOGMANAGERGLOBAL_H

#include <QtCore/QtGlobal>

#if defined(LOGMANAGER_LIBRARY)
#  define LOGMANAGER_EXPORT Q_DECL_EXPORT
#else
#  define LOGMANAGER_EXPORT Q_DECL_IMPORT
#endif

#endif
