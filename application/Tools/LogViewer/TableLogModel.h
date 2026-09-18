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
  
#pragma once

#include <QAbstractTableModel>
#include <QFile>
#include <QVector>
#include <QSharedPointer>
#include <QStringList>
#include <QPointer>
#include <QSet>
#include <QTimer>

#include <Common/LogManager/IEntry.h>

class TableLogModel : public QAbstractTableModel
{
   Q_OBJECT

public:
   enum Column {
      DATE_TIME = 0,
      SEVERITY = 1,
      MODULE_NAME = 2,
      THREAD_NAME = 3,
      SOURCE = 4,
      MESSAGE = 5
   };

   TableLogModel();

   int rowCount(const QModelIndex& parent = QModelIndex()) const override;
   int columnCount(const QModelIndex& parent = QModelIndex()) const override;
   QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
   QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

   void setDataSource(QFile* source);
   void setShowMultipleLines(bool enabled);
   void removeDataSource();

   LM::Severity getSeverity(int row) const;

   const QStringList& getSeverities() const;
   const QStringList& getModules() const;
   const QStringList& getThreads() const;

   void setFilter(const QStringList& severities, const QStringList& modules, const QStringList& threads);
   void resetFilter();

   void search(const QString& word);
   // Move strictly past 'from', wrapping around. An invalid index starts at
   // the first result (or last when searching backwards).
   std::pair<int, QModelIndex> nextResult(const QModelIndex& from, bool reverse = false) const;
   int searchResultNumber(const QModelIndex& index) const;
   bool inSearchResult(const QModelIndex& from) const;
   const QString& currentSearchTerm() const;
   int currentNbFoundItems() const;

   QString rowAsText(int row) const;

public slots:
   void setWatchingPause(bool pause);
   void refresh();

signals:
   /**
     * 'n' is the number of entry added.
     */
   void newLogEntries(int n);

   void newSeverity(QString);
   void newModule(QString);
   void newThread(QString);
   void loadingFinished();
   void dataSourceReset();
   void searchResultsChanged();

private:
   bool isFiltered(const QSharedPointer<LM::IEntry>& entry) const;
   void readLines();

   void clear();
   void rebuildSearch();

   QPointer<QFile> source;
   QTimer timer;
   QTimer readTimer;
   QByteArray pendingLine;
   bool readRequested = false;

   QVector<QSharedPointer<LM::IEntry>> entries;
   QVector<QSharedPointer<LM::IEntry>> filteredEntries;

   bool showMultipleLines;

   QSet<QString> severitiesFilter;
   QSet<QString> modulesFilter;
   QSet<QString> threadsFilter;

   QStringList severities;
   QStringList modules;
   QStringList threads;
   QSet<QString> knownSeverities;
   QSet<QString> knownModules;
   QSet<QString> knownThreads;

   // Search.
   QList<int> indexesFound; // Ordered from top to bottom, only rows are stored.
   QString currentSearch;

};
