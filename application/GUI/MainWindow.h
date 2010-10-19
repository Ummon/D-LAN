#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>
#include <QLabel>
#include <QItemDelegate>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include "PeerListModel.h"
#include "CoreConnection.h"
#include "WidgetChat.h"

namespace Ui {
   class MainWindow;
}

namespace GUI
{
   class MainWindow : public QMainWindow
   {
      Q_OBJECT
   public:
      explicit MainWindow(QWidget* parent = 0);
      ~MainWindow();

   private slots:
      void coreConnected();
      void coreDisconnected();
      void newState(const Protos::GUI::State& state);

   private:
      void addDefaultWidgets();

      Ui::MainWindow *ui;
      QLabel* lblStatusConnection;
      WidgetChat* widgetChat;

      PeerListModel peerListModel;

      CoreConnection coreConnection;
   };

   class PeerTableDelegate : public QItemDelegate
   {
      Q_OBJECT
   public:
      virtual void drawFocus(QPainter*, const QStyleOptionViewItem&, const QRect&) const {}
   };
}

#endif
