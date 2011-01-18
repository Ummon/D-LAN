#ifndef COMMON_SHAREDDIR_H
#define COMMON_SHAREDDIR_H

#include <QString>

#include <Common/Hash.h>

namespace Common
{
   /**
     * A little helper type.
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

      Common::Hash ID;
      QString path;
   };
}

#endif
