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
  
#include <priv/DownloadPredicate.h>
using namespace DM;

#include <IDownload.h>
#include <priv/Download.h>
#include <priv/FileDownload.h>
#include <priv/DirDownload.h>

bool IsDownloable::operator() (const Download* download) const
{
   const FileDownload* fileDownload = dynamic_cast<const FileDownload*>(download);
   return fileDownload && fileDownload->getStatus() != COMPLETE && fileDownload->getStatus() != DELETED;
}

bool IsADirectory::operator() (const Download* download) const
{
   return dynamic_cast<const DirDownload*>(download) != 0;
}

bool IsComplete::operator() (const Download* download) const
{
   return download->getStatus() == COMPLETE;
}

IsContainedInAList::IsContainedInAList(const QList<quint64>& downloadIDs) :
   downloadIDs(downloadIDs.toSet())
{
}

bool IsContainedInAList::operator() (const Download* download) const
{
   return this->downloadIDs.contains(download->getID());
}
