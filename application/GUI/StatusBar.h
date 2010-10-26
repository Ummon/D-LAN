#ifndef GUI_STATUSBAR_H
#define GUI_STATUSBAR_H

#include <QWidget>

#include <Protos/gui_protocol.pb.h>

#include <CoreConnection.h>

namespace Ui {
   class StatusBar;
}

namespace GUI
{
   class StatusBar : public QWidget
   {
      Q_OBJECT

   public:
      explicit StatusBar(CoreConnection& coreConnection, QWidget *parent = 0);
      ~StatusBar();

   private slots:
      void coreConnected();
      void coreDisconnected();
      void newState(const Protos::GUI::State& state);

   private:
      Ui::StatusBar *ui;

      CoreConnection& coreConnection;
   };
}

#endif
