#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>
#include <QLabel>
#include <QItemDelegate>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <PeerListModel.h>
#include <ChatModel.h>
#include <CoreConnection.h>

#include <WidgetSettings.h>
#include <WidgetChat.h>

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

   private:
      void addDefaultWidgets();

      Ui::MainWindow *ui;
      QLabel* lblStatusConnection;

      WidgetSettings* widgetSettings;
      WidgetChat* widgetChat;

      CoreConnection coreConnection;

      PeerListModel peerListModel;
      ChatModel chatModel;
   };

   class PeerTableDelegate : public QItemDelegate
   {
      Q_OBJECT
   public:
      virtual void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      virtual void drawFocus(QPainter*, const QStyleOptionViewItem&, const QRect&) const {}
   };
}

#endif
