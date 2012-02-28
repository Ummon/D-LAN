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

bool IsDownloable::operator() (Download* download)
{
   FileDownload* fileDownload = dynamic_cast<FileDownload*>(download);
   return fileDownload && fileDownload->getStatus() != COMPLETE && fileDownload->getStatus() != DELETED;
}

bool IsADirectory::operator() (Download* download)
{
   return dynamic_cast<DirDownload*>(download) != 0;
}

bool IsComplete::operator() (Download* download)
{
   return download->getStatus() == COMPLETE;
}

IsContainedInAList::IsContainedInAList(QList<quint64> downloadIDs) :
   downloadIDs(downloadIDs.toSet())
{
}

bool IsContainedInAList::operator() (Download* download)
{
   return this->downloadIDs.contains(download->getID());
}
