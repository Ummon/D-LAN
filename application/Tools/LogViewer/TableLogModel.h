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
  
#ifndef TABLELOGMODEL_H
#define TABLELOGMODEL_H

#include <QAbstractTableModel>
#include <QFile>
#include <QVector>
#include <QSharedPointer>
#include <QStringList>
#include <QFileSystemWatcher>
#include <QSocketNotifier>
#include <QTimer>

#include <Common/LogManager/IEntry.h>

class TableLogModel : public QAbstractTableModel
{
   Q_OBJECT
public:
   TableLogModel();

   int rowCount(const QModelIndex& parent = QModelIndex()) const;
   int columnCount(const QModelIndex& parent = QModelIndex()) const;
   QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
   QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;

   void setDataSource(QFile* source);
   void removeDataSource();

   LM::Severity getSeverity(int row) const;

   const QStringList& getSeverities() const;
   const QStringList& getModules() const;
   const QStringList& getThreads() const;

   bool isFiltered(int num, const QStringList& severities, const QStringList& modules, const QStringList& threads) const;

public slots:
   void setWatchingPause(bool pause);

signals:
   /**
     * 'n' is the number of entry added.
     */
   void newLogEntries(int n);

   void newSeverity(QString);
   void newModule(QString);
   void newThread(QString);

private slots:
   void fileChanged();

private:
   void readLines();

   void clear();

   QFile* source;
   QTimer timer;

   QVector< QSharedPointer<LM::IEntry> > entries;

   QStringList severities;
   QStringList modules;
   QStringList threads;
};

#endif
