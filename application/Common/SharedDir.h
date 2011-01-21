#ifndef COMMON_SHAREDDIR_H
#define COMMON_SHAREDDIR_H

#include <QString>
#include <QMetaType>

#include <Common/Hash.h>

namespace Common
{
   /**
     * A little helper type.
     * A tuple (ID, path) that identify a SharedDirectory.
     * Used by FM::IFileManager.
     */
   struct SharedDir
   {
      /**
        * Build a null SharedDir.
        */
      SharedDir() {};

      SharedDir(const Common::Hash& ID, const QString& path, quint64 size, quint64 freeSpace) :
         ID(ID), path(path), size(size), freeSpace(freeSpace) {}

      bool isNull() const { return ID.isNull(); }

      bool operator==(const SharedDir& other) const { return this->ID == other.ID; }
      bool equalTo(const SharedDir& other) const { return this->ID == other.ID && this->path == other.path && this->size == other.size && this->freeSpace == other.freeSpace; }

      Common::Hash ID; ///< The unique identifier of the shared directory.
      QString path; ///< The absolute path of the shared directory.
      quint64 size;
      quint64 freeSpace;
   };
}

Q_DECLARE_METATYPE(Common::SharedDir)

#endif
