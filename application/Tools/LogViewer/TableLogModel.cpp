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

#include <QElapsedTimer>
#include <algorithm>

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
   connect(&this->timer, &QTimer::timeout, this, &TableLogModel::refresh);
   this->readTimer.setSingleShot(true);
   this->readTimer.setInterval(0);
   connect(&this->readTimer, &QTimer::timeout, this, &TableLogModel::readLines);
}

int TableLogModel::rowCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : this->filteredEntries.count();
}

int TableLogModel::columnCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : 6;
}

QVariant TableLogModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() < 0 || index.row() >= this->filteredEntries.count())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      {
         const auto& entry = this->filteredEntries[index.row()];

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
            const auto& entry = this->filteredEntries[index.row()];
            // Force HTML detection with <qt> tag.
            return QVariant("<qt>" + entry->getMessageWithLF() + "</qt>");
         }
         return {};
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
   emit dataSourceReset();
   this->refresh();
}

void TableLogModel::setShowMultipleLines(bool enabled)
{
   if (this->showMultipleLines == enabled)
      return;

   this->showMultipleLines = enabled;
   if (!this->filteredEntries.isEmpty())
      emit dataChanged(this->index(0, MESSAGE), this->index(this->filteredEntries.size() - 1, MESSAGE));
}

void TableLogModel::removeDataSource()
{
   this->clear();
   this->source = nullptr;
}

LM::Severity TableLogModel::getSeverity(int row) const
{
   if (row < 0 || row >= this->filteredEntries.count())
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
   const QSet<QString> newSeverities(severities.begin(), severities.end());
   const QSet<QString> newModules(modules.begin(), modules.end());
   const QSet<QString> newThreads(threads.begin(), threads.end());
   if (this->severitiesFilter == newSeverities && this->modulesFilter == newModules && this->threadsFilter == newThreads)
      return;
   this->beginResetModel();

   this->severitiesFilter = newSeverities;
   this->modulesFilter = newModules;
   this->threadsFilter = newThreads;

   this->filteredEntries.clear();

   for (const auto& entry : std::as_const(this->entries))
   {
      if (!this->isFiltered(entry))
         this->filteredEntries << entry;
   }

   this->rebuildSearch();

   this->endResetModel();
   emit searchResultsChanged();
}

void TableLogModel::resetFilter()
{
   this->setFilter(this->severities, this->modules, this->threads);
}

void TableLogModel::search(const QString& word)
{
   if (this->currentSearch == word)
      return;
   this->currentSearch = word;
   this->rebuildSearch();
   // Highlighting changes painting only; do not invalidate document layouts/heights.
   emit searchResultsChanged();
}

void TableLogModel::rebuildSearch()
{
   this->indexesFound.clear();
   if (this->currentSearch.isEmpty())
      return;

   for (int row = 0; row < this->filteredEntries.size(); ++row)
   {
      if (this->filteredEntries[row]->getMessage().contains(this->currentSearch, Qt::CaseInsensitive))
         this->indexesFound.append(row);
   }
}

std::pair<int, QModelIndex> TableLogModel::nextResult(const QModelIndex& from, bool reverse) const
{
   const int s = this->indexesFound.size();
   if (s == 0)
      return std::make_pair(0, QModelIndex());

   const auto begin = this->indexesFound.cbegin();
   const auto end = this->indexesFound.cend();
   int pos = reverse ? s - 1 : 0;
   if (from.isValid())
   {
      if (reverse)
      {
         const auto found = std::lower_bound(begin, end, from.row());
         pos = found == begin ? s - 1 : int(found - begin) - 1;
      }
      else
      {
         const auto found = std::upper_bound(begin, end, from.row());
         pos = found == end ? 0 : int(found - begin);
      }
   }
   return std::make_pair(pos, this->index(this->indexesFound[pos], MESSAGE));
}

int TableLogModel::searchResultNumber(const QModelIndex& index) const
{
   if (!index.isValid())
      return 0;
   const auto found = std::lower_bound(this->indexesFound.cbegin(), this->indexesFound.cend(), index.row());
   return found != this->indexesFound.cend() && *found == index.row() ? int(found - this->indexesFound.cbegin()) + 1 : 0;
}

bool TableLogModel::inSearchResult(const QModelIndex& index) const
{
   return this->searchResultNumber(index) != 0;
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
   {
      this->timer.stop();
      this->readTimer.stop();
      this->readRequested = false;
   }
   else
   {
      this->timer.start();
      this->refresh();
   }
}

void TableLogModel::refresh()
{
   if (this->source && this->source->isOpen() && !this->readTimer.isActive())
   {
      this->readRequested = true;
      this->readTimer.start();
   }
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
   if (!this->source || !this->source->isOpen() || !this->readRequested)
      return;

   // Detect truncation for automatic updates as well as manual refresh.
   if (this->source->size() < this->source->pos())
   {
      QFile* source = this->source.data();
      if (source->seek(0))
         this->setDataSource(source);
      return;
   }

   // Read complete UTF-8 records. A writer may stop in the middle of a line or a
   // codepoint; retain those bytes until the newline arrives. Chunk large records
   // as well, so reading an unfinished message does not monopolize the UI thread.
   QElapsedTimer elapsed;
   elapsed.start();
   QVector<QSharedPointer<LM::IEntry>> visible;
   int linesRead = 0;
   while (!this->source->atEnd() && linesRead < 256 && elapsed.elapsed() < 8)
   {
      const QByteArray bytes = this->source->readLine(64 * 1024);
      if (bytes.isEmpty())
      {
         this->readRequested = false;
         break;
      }
      this->pendingLine += bytes;
      ++linesRead;
      if (!this->pendingLine.endsWith('\n'))
         continue;
      this->pendingLine.chop(1);
      if (this->pendingLine.endsWith('\r'))
         this->pendingLine.chop(1);
      QString line = QString::fromUtf8(this->pendingLine);
      this->pendingLine.clear();
      if (line.trimmed().isEmpty())
         continue;

      try
      {
         QSharedPointer<LM::IEntry> entry = LM::Builder::decode(line);
         this->entries << entry;

         if (!this->knownSeverities.contains(entry->getSeverityStr()))
         {
            this->knownSeverities.insert(entry->getSeverityStr());
            this->severities << entry->getSeverityStr();
            this->severitiesFilter << this->severities.constLast();

            emit newSeverity(entry->getSeverityStr());
         }

         if (!this->knownModules.contains(entry->getName()))
         {
            this->knownModules.insert(entry->getName());
            this->modules << entry->getName();
            this->modulesFilter << this->modules.constLast();

            emit newModule(entry->getName());
         }

         if (!this->knownThreads.contains(entry->getThread()))
         {
            this->knownThreads.insert(entry->getThread());
            this->threads << entry->getThread();
            this->threadsFilter << this->threads.constLast();

            emit newThread(entry->getThread());
         }

         if (!this->isFiltered(entry))
            visible << entry;
      }
      catch (LM::MalformedEntryLog&)
      {
         L_WARN(QString("Malformed line ignored: %1").arg(line));
      }
   }

   if (!visible.isEmpty())
   {
      const int first = this->filteredEntries.size();
      this->beginInsertRows(QModelIndex(), first, first + visible.size() - 1);
      this->filteredEntries << visible;
      if (!this->currentSearch.isEmpty())
         for (int row = first; row < this->filteredEntries.size(); ++row)
            if (this->filteredEntries[row]->getMessage().contains(this->currentSearch, Qt::CaseInsensitive))
               this->indexesFound.append(row);
      this->endInsertRows();
      emit newLogEntries(visible.size());
      if (!this->currentSearch.isEmpty())
         emit searchResultsChanged();
   }

   if (this->readRequested && this->source && !this->source->atEnd())
      this->readTimer.start();
   else
   {
      this->readRequested = false;
      emit loadingFinished();
   }
}

void TableLogModel::clear()
{
   this->readTimer.stop();
   this->readRequested = false;
   this->pendingLine.clear();
   this->source = nullptr;
   this->beginResetModel();
   this->severities.clear();
   this->modules.clear();
   this->threads.clear();
   this->knownSeverities.clear();
   this->knownModules.clear();
   this->knownThreads.clear();

   this->severitiesFilter.clear();
   this->modulesFilter.clear();
   this->threadsFilter.clear();

   this->entries.clear();
   this->filteredEntries.clear();
   this->indexesFound.clear();
   this->endResetModel();
   emit searchResultsChanged();
}
