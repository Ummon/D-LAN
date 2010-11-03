#ifndef GUI_WIDGETSETTINGS_H
#define GUI_WIDGETSETTINGS_H

#include <QWidget>
#include <QDir>
#include <QAbstractListModel>

#include <CoreConnection.h>

namespace Ui {
    class WidgetSettings;
}

namespace GUI
{
   class DirListModel : public QAbstractListModel
   {
   public:
      void setDirs(const QStringList& dirs);
      void addDir(const QString& dir);
      void rmDir(int row);
      const QStringList& getDirs() const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   private:
      QStringList dirs;
   };

   class WidgetSettings : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetSettings(CoreConnection& coreConnection, QWidget *parent = 0);
      ~WidgetSettings();

      void coreConnected();
      void coreDisconnected();

   private slots:
      void newState(const Protos::GUI::State& state);
      void saveCoreSettings();
      void saveGUISettings();

      void addIncoming();
      void addShared();
      void removeIncoming();
      void removeShared();

      void resetCoreAddress();

      QString askADirectory();

   private:
      Ui::WidgetSettings *ui;

      DirListModel incomingDirsModel;
      DirListModel sharedDirsModel;

      CoreConnection& coreConnection;
   };
}

#endif
