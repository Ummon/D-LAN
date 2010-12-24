#ifndef GUI_DIRLISTMODEL_H
#define GUI_DIRLISTMODEL_H

#include <QString>
#include <QStringList>
#include <QAbstractListModel>

namespace GUI
{
   class DirListModel : public QAbstractListModel
   {
   public:
      void setDirs(const QStringList& dirs);
      void addDir(const QString& dir);
      void addDirs(const QStringList& dirs);
      void rmDir(int row);
      const QStringList& getDirs() const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   private:
      QStringList dirs;
   };
}

#endif
