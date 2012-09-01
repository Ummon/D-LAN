#ifndef GUI_ROOMSDOCK_H
#define GUI_ROOMSDOCK_H

#include <QDockWidget>
#include <QSharedPointer>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Chat/RoomsModel.h>
#include <Chat/RoomsDelegate.h>

namespace Ui {
   class RoomsDock;
}

namespace GUI
{
   class RoomsDock : public QDockWidget
   {
      Q_OBJECT

   public:
      explicit RoomsDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = 0);
      ~RoomsDock();

   signals:
      void roomJoined(const QString&);

   private slots:
      void joinRoom();

      void coreConnected();
      void coreDisconnected(bool force);

   private:
      Ui::RoomsDock* ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      RoomsModel roomsModel;
      RoomsDelegate roomsDelegate;
   };
}

#endif
