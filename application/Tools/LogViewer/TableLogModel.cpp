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
  
#include <TableLogModel.h>

#include <QTextStream>

#include <Common/LogManager/LogMacros.h>
#include <Common/LogManager/Builder.h>
#include <Common/LogManager/Exceptions.h>

/**
  * @class TableLogModel
  *
  * Access to the file data log, read it and organize it for the views.
  */

TableLogModel::TableLogModel() :
   source(nullptr), showMultipleLines(false)
{
   this->timer.setInterval(500);
   connect(&this->timer, &QTimer::timeout, this, &TableLogModel::fileChanged);
}

int TableLogModel::rowCount(const QModelIndex& parent) const
{
   return this->filteredEntries.count();
}

int TableLogModel::columnCount(const QModelIndex& parent) const
{
   return 6;
}

QVariant TableLogModel::data(const QModelIndex& index, int role) const
{
   if (index.row() >= this->filteredEntries.count())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      {
         QSharedPointer<LM::IEntry> entry = this->filteredEntries[index.row()];

         switch (index.column())
         {
         case DATE_TIME: return entry->getDateStr();
         case SEVERITY: return entry->getSeverityStr();
         case MODULE_NAME: return entry->getName();
         case THREAD_NAME: return entry->getThread();
         case SOURCE: return entry->getSource();
         case MESSAGE:
            {
               if (this->showMultipleLines)
                  return entry->getMessageWithLF();
               else
               {
                  // Replace <pre> by <span> to get inline code.
                  QString message = entry->getMessageWithLF();
                  message.replace("<pre", "<span");
                  message.replace("pre>", "span>");
                  return message;
               }
            }
         default: return QVariant();
         }
      }
   case Qt::ToolTipRole:
      {
         if (index.column() == MESSAGE)
         {
            QSharedPointer<LM::IEntry> entry = this->filteredEntries[index.row()];
            // Force HTML detection with <qt> tag.
            return QVariant("<qt>" + entry->getMessageWithLF() + "</qt>");
         }
      }

   case Qt::TextAlignmentRole:
       return int(Qt::AlignLeft | Qt::AlignTop);
   }

   return QVariant();
}

QVariant TableLogModel::headerData(int section, Qt::Orientation orientation, int role) const
{
   if (role != Qt::DisplayRole || orientation != Qt::Horizontal)
      return QVariant();

   switch (section)
   {
   case DATE_TIME: return "Date+Time";
   case SEVERITY: return "Severity";
   case MODULE_NAME: return "Module";
   case THREAD_NAME: return "Thread";
   case SOURCE: return "Source";
   case MESSAGE: return "Message";
   default: return QVariant();
   }
}

void TableLogModel::setDataSource(QFile* source)
{
   this->clear();
   this->source = source;
   this->readLines();
}

void TableLogModel::setShowMultipleLines(bool enabled)
{
   if (this->showMultipleLines == enabled)
      return;

   this->showMultipleLines = enabled;
   emit(dataChanged(this->index(0, 5), this->index(this->filteredEntries.size()-1, 5)));
}

void TableLogModel::removeDataSource()
{
   this->clear();
   this->source = nullptr;
}

LM::Severity TableLogModel::getSeverity(int row) const
{
   if (row >= this->filteredEntries.count())
      return LM::SV_UNKNOWN;
   return this->filteredEntries[row]->getSeverity();
}

const QStringList& TableLogModel::getSeverities() const
{
   return this->severities;
}

const QStringList& TableLogModel::getModules() const
{
   return this->modules;
}

const QStringList& TableLogModel::getThreads() const
{
   return this->threads;
}

void TableLogModel::setFilter(const QStringList& severities, const QStringList& modules, const QStringList& threads)
{
   this->beginResetModel();

   this->severitiesFilter = severities;
   this->modulesFilter = modules;
   this->threadsFilter = threads;

   this->filteredEntries.clear();

   for (const auto& entry : std::as_const(this->entries))
   {
      if (!this->isFiltered(entry))
         this->filteredEntries << entry;
   }

   if (!this->currentSearch.isEmpty())
      this->search(this->currentSearch);

   this->endResetModel();
}

void TableLogModel::resetFilter()
{
   this->setFilter(this->severities, this->modules, this->threads);
}

void TableLogModel::search(const QString& word)
{
   this->indexesFound.clear();
   this->currentSearch = word.toLower();

   for (int row = 0; row < this->filteredEntries.size(); ++row)
   {
      if (this->filteredEntries[row]->getMessage().toLower().contains(this->currentSearch))
         this->indexesFound.append(row);
   }
}

std::pair<int, QModelIndex> TableLogModel::nextResult(const QModelIndex& from, bool reverse) const
{
   const int s = this->indexesFound.size();
   if (s == 0)
      return std::make_pair(0, QModelIndex());

   const int fromRow = !from.isValid() ? 0 : from.row();

   int start = 0;
   int end = s;

   while (start < end) {
      const int i = (end - start) / 2 + start;
      const int currentRow = this->indexesFound[i];
      if (currentRow == fromRow)
      {
         int pos = i % s;
         return std::make_pair(pos, this->createIndex(this->indexesFound[pos], MESSAGE));
      }
      else if (currentRow > fromRow)
         end = i;
      else
         start = i + 1;
   }

   if (!reverse && start < s && this->indexesFound[start] < fromRow)
      start += 1;

   if (reverse && start < s && this->indexesFound[start] > fromRow)
   {
      start -= 1;
      if (start < 0)
         start = s - 1;
   }

   int pos = start % s;
   return std::make_pair(pos, this->createIndex(this->indexesFound[start % s], MESSAGE));
}

bool TableLogModel::inSearchResult(const QModelIndex& index) const
{
   return this->nextResult(index).second.row() == index.row();
}

const QString& TableLogModel::currentSearchTerm() const
{
   return this->currentSearch;
}

int TableLogModel::currentNbFoundItems() const
{
   return this->indexesFound.size();
}

QString TableLogModel::rowAsText(int row) const
{
   if (row < 0 || row >= this->filteredEntries.size())
      return QString();

   const auto& entry = this->filteredEntries[row];

   return
      entry->getDateStr() % " | " %
      entry->getName() % " | " %
      entry->getThread() % " | " %
      entry->getSource() % " | " %
      entry->getMessageWithLF();
}

void TableLogModel::setWatchingPause(bool pause)
{
   if (pause)
      this->timer.stop();
   else
      this->timer.start();
}

void TableLogModel::fileChanged()
{
   this->readLines();
}

bool TableLogModel::isFiltered(const QSharedPointer<LM::IEntry>& entry) const
{
   return
      !(
         this->severitiesFilter.contains(entry->getSeverityStr()) &&
         this->modulesFilter.contains(entry->getName()) &&
         this->threadsFilter.contains(entry->getThread())
      );
}

void TableLogModel::readLines()
{
   if (!this->source)
      return;

   QTextStream stream(this->source);
   stream.setEncoding(QStringConverter::Utf8);

   const int count = this->filteredEntries.count();

   QString line;
   while (line = stream.readLine(), !line.isNull())
   {
      line = line.trimmed();
      if (line.isEmpty())
         continue;

      try
      {
         QSharedPointer<LM::IEntry> entry = LM::Builder::decode(line);
         this->entries << entry;
         if (!this->isFiltered(entry))
            this->filteredEntries << entry;

         if (!this->severities.contains(entry->getSeverityStr()))
         {
            this->severities << entry->getSeverityStr();
            this->severitiesFilter << this->severities.constLast();

            emit newSeverity(entry->getSeverityStr());
         }

         if (!this->modules.contains(entry->getName()))
         {
            this->modules << entry->getName();
            this->modulesFilter << this->modules.constLast();

            emit newModule(entry->getName());
         }

         if (!this->threads.contains(entry->getThread()))
         {
            this->threads << entry->getThread();
            this->threadsFilter << this->threads.constLast();

            emit newThread(entry->getThread());
         }
      }
      catch (LM::MalformedEntryLog&)
      {
         L_WARN(QString("Malformed line ignored: %1").arg(line));
      }
   }

   if (this->filteredEntries.count() - count <= 0)
      return;

   this->beginInsertRows(QModelIndex(), count, this->filteredEntries.count() - 1);
   this->endInsertRows();

   emit(newLogEntries(this->filteredEntries.count() - count));
}

void TableLogModel::clear()
{
   if (this->entries.empty())
      return;

   this->beginRemoveRows(QModelIndex(), 0, this->filteredEntries.count() - 1);
   this->severities.clear();
   this->modules.clear();
   this->threads.clear();

   this->severitiesFilter.clear();
   this->modulesFilter.clear();
   this->threadsFilter.clear();

   this->entries.clear();
   this->filteredEntries.clear();
   this->endRemoveRows();
}
