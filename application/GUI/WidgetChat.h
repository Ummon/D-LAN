#ifndef GUI_WIDGETCHAT_H
#define GUI_WIDGETCHAT_H

#include <QWidget>

namespace Ui {
   class WidgetChat;
}

namespace GUI
{
   class WidgetChat : public QWidget
   {
      Q_OBJECT

   public:
      explicit WidgetChat(QWidget *parent = 0);
      ~WidgetChat();

   private:
      Ui::WidgetChat *ui;
   };
}
#endif
