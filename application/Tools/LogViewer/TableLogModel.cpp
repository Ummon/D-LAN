#include "TableLogModel.h"

#include <QTextStream>

#include <Common/LogManager/Builder.h>
#include <Common/LogManager/Exceptions.h>

/**
  * @class TableLogModel
  * Acess to the file data log, read it and organize it for the views.
  */

TableLogModel::TableLogModel() :
   source(0)
{
   this->timer.setInterval(500);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(fileChanged()));
}

int TableLogModel::rowCount(const QModelIndex& parent) const
{
   return this->entries.count();
}

int TableLogModel::columnCount(const QModelIndex& parent) const
{
   return 6;
}

QVariant TableLogModel::data(const QModelIndex& index, int role) const
{
   if (role != Qt::DisplayRole || index.row() >= this->entries.count())
      return QVariant();

   QSharedPointer<LM::IEntry> entry = this->entries[index.row()];

   switch (index.column())
   {
   case 0 : return entry->getDateStr();
   case 1 : return entry->getSeverityStr();
   case 2 : return entry->getName();
   case 3 : return entry->getThread();
   case 4 : return entry->getSource();
   case 5 : return entry->getMessage().replace("<lf>", "\n");
   default : return QVariant();
   }
}

QVariant TableLogModel::headerData(int section, Qt::Orientation orientation, int role) const
{
   if (role != Qt::DisplayRole || orientation != Qt::Horizontal)
      return QVariant();

   switch (section)
   {
   case 0 : return "Date+Time";
   case 1 : return "Severity";
   case 2 : return "Module";
   case 3 : return "Thread";
   case 4 : return "Source";
   case 5 : return "Message";
   default : return QVariant();
   }
}

void TableLogModel::setDataSource(QFile* source)
{
   this->clear();
   this->source = source;
   this->readLines();
}

void TableLogModel::removeDataSource()
{
   this->clear();
   this->source = 0;
}

LM::Severity TableLogModel::getSeverity(int row) const
{
   if (row >= this->entries.count())
      return LM::SV_UNKNOWN;
   return this->entries[row]->getSeverity();
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

bool TableLogModel::isFiltered(int num, const QStringList& severities, const QStringList& modules, const QStringList& threads) const
{
   if (num >= this->entries.count())
      return false;
   return !(
      severities.contains(this->entries[num]->getSeverityStr()) &&
      modules.contains(this->entries[num]->getName()) &&
      threads.contains(this->entries[num]->getThread())
   );
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

void TableLogModel::readLines()
{
   if (!this->source)
      return;

   QTextStream stream(this->source);
   stream.setCodec("UTF-8");

   int count = this->entries.count();

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

         if (!this->severities.contains(entry->getSeverityStr()))
         {
            this->severities << entry->getSeverityStr();
            emit newSeverity(entry->getSeverityStr());
         }

         if (!this->modules.contains(entry->getName()))
         {
            this->modules << entry->getName();
            emit newModule(entry->getName());
         }

         if (!this->threads.contains(entry->getThread()))
         {
            this->threads << entry->getThread();
            emit newThread(entry->getThread());
         }
      }
      catch (LM::MalformedEntryLog&)
      {
         // Malformed line are silently ignored.
      }
   }

   if (this->entries.count() - count <= 0)
      return;

   this->beginInsertRows(QModelIndex(), count, this->entries.count() - 1);
   this->endInsertRows();

   emit(newLogEntries(this->entries.count() - count));
}

void TableLogModel::clear()
{
   if (this->entries.empty())
      return;

   this->beginRemoveRows(QModelIndex(), 0, this->entries.count() - 1);
   this->severities.clear();
   this->modules.clear();
   this->threads.clear();
   this->entries.clear();
   this->endRemoveRows();
}
