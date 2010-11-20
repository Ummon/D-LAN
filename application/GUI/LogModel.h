#ifndef GUI_LOGMODEL_H
#define GUI_LOGMODEL_H

#include <QAbstractTableModel>

#include <Common/LogManager/IEntry.h>
#include <Common/LogManager/ILoggerHook.h>

#include <CoreConnection.h>

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
