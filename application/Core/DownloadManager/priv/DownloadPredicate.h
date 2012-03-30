/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef DOWNLOADMANAGER_DOWNLOADPREDICATE_H
#define DOWNLOADMANAGER_DOWNLOADPREDICATE_H

#include <QSet>
#include <QList>

namespace DM
{
   class Download;

   struct DownloadPredicate
   {
      virtual bool operator() (const Download* download) const = 0;
      virtual ~DownloadPredicate() {}
   };

   struct IsDownloable : public DownloadPredicate
   {
      bool operator() (const Download* download) const;
   };

   struct IsADirectory : public DownloadPredicate
   {
      bool operator() (const Download* download) const;
   };

   struct IsComplete : public DownloadPredicate
   {
      bool operator() (const Download* download) const;
   };

   struct IsContainedInAList : public DownloadPredicate
   {
      IsContainedInAList(const QList<quint64>& downloadIDs);
      bool operator() (const Download* download) const;

   private:
      QSet<quint64> downloadIDs; // We us a QSet to decrease the complexity.
   };
}

#endif
