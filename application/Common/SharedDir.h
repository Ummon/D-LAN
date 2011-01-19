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

      SharedDir(const Common::Hash& ID, const QString& path) : ID(ID), path(path) {}

      bool isNull() const { return ID.isNull(); }

      bool operator==(const SharedDir& other) const { return this->ID == other.ID; }

      Common::Hash ID; ///< The unique identifier of the shared directory.
      QString path; ///< The absolute path of the shared directory.
   };
}

Q_DECLARE_METATYPE(Common::SharedDir)

#endif
