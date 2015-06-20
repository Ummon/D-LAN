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
  
#include <Log/LogModel.h>
using namespace GUI;

#include <Common/Settings.h>
#include <Common/LogManager/Builder.h>

LogModel::LogModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection)
{
   connect(this->coreConnection.data(), RCC::ICoreConnection::newLogMessages, this, newLogEntries);

   this->loggerHook = LM::Builder::newLoggerHook(LM::Severity(LM::SV_FATAL_ERROR | LM::SV_ERROR | LM::SV_END_USER | LM::SV_WARNING));
   connect(this->loggerHook.data(), LM::ILoggerHook::newLogEntry, this, newLogEntry);
}

int LogModel::rowCount(const QModelIndex& /*parent*/) const
{
   return this->entries.size();
}

int LogModel::columnCount(const QModelIndex& /*parent*/) const
{
   return 2;
}

QVariant LogModel::data(const QModelIndex& index, int role) const
{
   if (role != Qt::DisplayRole || index.row() >= this->entries.size())
      return QVariant();

   const QSharedPointer<LM::IEntry>& entry = this->entries[index.row()];

   switch (index.column())
   {
   case 0:
      return entry->getDateStr(false);

   case 1:
      {
         QString message;
         switch (entry->getSeverity())
         {
         case LM::SV_FATAL_ERROR:
            message.append("[Fatal Error] ");
            break;
         case LM::SV_ERROR:
            message.append("[Error] ");
            break;
         default:;
         }
         message.append(entry->getMessage());
         return message;
      }

   default:
      return QVariant();
   }
}

LM::Severity LogModel::getSeverity(int row) const
{
   if (row >= this->entries.size())
      return LM::SV_UNKNOWN;
   return this->entries[row]->getSeverity();
}

void LogModel::newLogEntry(QSharedPointer<LM::IEntry> entry)
{
   this->newLogEntries(QList<QSharedPointer<LM::IEntry>> { entry });
}

void LogModel::newLogEntries(const QList<QSharedPointer<LM::IEntry>>& entries)
{
   QList<QSharedPointer<LM::IEntry>> filteredEntries;

   // Report Warnings only in DEBUG mode and do not repeat several same messages.
   for (QListIterator<QSharedPointer<LM::IEntry>> i(entries); i.hasNext();)
   {
      const QSharedPointer<LM::IEntry>& entry = i.next();
#ifndef DEBUG
      if (entry->getSeverity() != LM::SV_WARNING)
#endif
      {
         if (filteredEntries.isEmpty() || entry->getMessage() != filteredEntries.last()->getMessage())
            filteredEntries << entry;
      }
   }

   if (!filteredEntries.isEmpty() && !this->entries.isEmpty() && filteredEntries.last()->getMessage() == this->entries.last()->getMessage())
      filteredEntries.removeLast();

   if (filteredEntries.isEmpty())
      return;

   this->beginInsertRows(QModelIndex(), this->entries.size(), this->entries.size() + filteredEntries.size() - 1);
   this->entries << filteredEntries;
   this->endInsertRows();

   static const quint32 MAX_LOG_MESSAGE_DISPLAYED = SETTINGS.get<quint32>("max_log_message_displayed");
   if (quint32(this->entries.size()) > MAX_LOG_MESSAGE_DISPLAYED)
   {
      this->beginRemoveRows(QModelIndex(), 0, quint32(this->entries.size()) - MAX_LOG_MESSAGE_DISPLAYED - 1);
      this->entries.erase(this->entries.begin(), this->entries.begin() + (quint32(this->entries.size()) - MAX_LOG_MESSAGE_DISPLAYED));
      this->endRemoveRows();
   }
}
