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
  
#ifndef GUI_LOGMODEL_H
#define GUI_LOGMODEL_H

#include <QAbstractTableModel>

#include <Common/LogManager/IEntry.h>
#include <Common/LogManager/ILoggerHook.h>

#include <CoreConnection/CoreConnection.h>

namespace GUI
{
   class LogModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      LogModel(CoreConnection& coreConnection);

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      LM::Severity getSeverity(int row) const;

   private slots:
      void newLogEntry(QSharedPointer<const LM::IEntry> entry);

   private:
      CoreConnection& coreConnection;
      QSharedPointer<LM::ILoggerHook> loggerHook;
      QList< QSharedPointer<const LM::IEntry> > entries;
   };
}

#endif
