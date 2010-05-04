#ifndef TABLELOGMODEL_H
#define TABLELOGMODEL_H

#include <QAbstractTableModel>
#include <QFile>
#include <QVector>
#include <QSharedPointer>
#include <QStringList>

#include <Common/LogManager/IEntry.h>

class TableLogModel : public QAbstractTableModel
{
public:
   TableLogModel();

   int rowCount(const QModelIndex& parent = QModelIndex()) const;
   int columnCount(const QModelIndex& parent = QModelIndex()) const;
   QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
   QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const;

   void setDataSource(QFile* source);
   void removeDataSource();

   const QStringList& getSeverities();
   const QStringList& getModules();
   const QStringList& getThreads();

private:
   void readLines();

   void clear();

   QFile* source;

   QVector< QSharedPointer<LM::IEntry> > entries;
   QStringList severities;
   QStringList modules;
   QStringList threads;
};

#endif
