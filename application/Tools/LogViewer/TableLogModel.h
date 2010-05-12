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
