#ifndef GUI_DOWNLOADMENU_H
#define GUI_DOWNLOADMENU_H

#include <QObject>
#include <QSharedPointer>
#include <QPoint>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Settings/DirListModel.h>

namespace GUI
{
   class DownloadMenu : public QObject
   {
      Q_OBJECT
   public:
      DownloadMenu(QSharedPointer<RCC::ICoreConnection> coreConnection, const DirListModel& sharedDirsModel);
      void show(const QPoint& globalPosition);

   signals:
      void downloadTo(const Common::Hash&, const QString&);

   private slots:
      void actionTriggered();

   private:
      QSharedPointer<RCC::ICoreConnection> coreConnection;
      const DirListModel& sharedDirsModel;
   };
}

#endif
