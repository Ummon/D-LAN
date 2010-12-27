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
  
#include <Log/LogModel.h>
using namespace GUI;

#include <Common/Settings.h>
#include <Common/LogManager/Builder.h>

LogModel::LogModel(CoreConnection& coreConnection)
   : coreConnection(coreConnection)
{
   connect(&this->coreConnection, SIGNAL(newLogMessage(QSharedPointer<const LM::IEntry>)), this, SLOT(newLogEntry(QSharedPointer<const LM::IEntry>)));

   this->loggerHook = LM::Builder::newLoggerHook(LM::Severity(LM::SV_FATAL_ERROR | LM::SV_ERROR | LM::SV_END_USER | LM::SV_WARNING));
   connect(this->loggerHook.data(), SIGNAL(newLogEntry(QSharedPointer<const LM::IEntry>)), this, SLOT(newLogEntry(QSharedPointer<const LM::IEntry>)));
}

int LogModel::rowCount(const QModelIndex& parent) const
{
   return this->entries.size();
}

int LogModel::columnCount(const QModelIndex& parent) const
{
   return 2;
}

QVariant LogModel::data(const QModelIndex& index, int role) const
{
   if (role != Qt::DisplayRole || index.row() >= this->entries.size())
      return QVariant();

   QSharedPointer<const LM::IEntry> entry = this->entries[index.row()];

   QString messagePrefix;
   switch(entry->getSeverity())
   {
   case LM::SV_FATAL_ERROR:
      messagePrefix = "[Fatal Error] ";
      break;
   case LM::SV_ERROR:
      messagePrefix = "[Error] ";
      break;
   default:;
   }

   switch (index.column())
   {
   case 0: return entry->getDateStr(false);
   case 1: return messagePrefix + entry->getMessage();
   default: return QVariant();
   }
}

LM::Severity LogModel::getSeverity(int row) const
{
   if (row >= this->entries.size())
      return LM::SV_UNKNOWN;
   return this->entries[row]->getSeverity();
}

void LogModel::newLogEntry(QSharedPointer<const LM::IEntry> entry)
{
   // Report Warnings only in DEBUG mode.
#ifndef DEBUG
   if (entry->getSeverity() == LM::SV_WARNING)
      return;
#endif

   // Do not repeat several same messages.
   if (!this->entries.isEmpty() && this->entries.last()->getMessage() == entry->getMessage())
      return;

   this->beginInsertRows(QModelIndex(), this->entries.size(), this->entries.size());
   this->entries << entry;
   this->endInsertRows();

   if (static_cast<quint32>(this->entries.size()) > SETTINGS.get<quint32>("max_log_message_displayed"))
   {
      this->beginRemoveRows(QModelIndex(), 0, 0);
      this->entries.removeFirst();
      this->endRemoveRows();
   }
}
