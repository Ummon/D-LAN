#ifndef FILEMANAGER_GLOBAL_H
#define FILEMANAGER_GLOBAL_H

#include <QtCore/qglobal.h>

#if defined(FILEMANAGER_LIBRARY)
#  define FILEMANAGER_EXPORT Q_DECL_EXPORT
#else
#  define FILEMANAGER_EXPORT Q_DECL_IMPORT
#endif

#endif
