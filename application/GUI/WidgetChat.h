#ifndef GUI_WIDGETCHAT_H
#define GUI_WIDGETCHAT_H

#include <QWidget>
#include <QItemDelegate>

#include <CoreConnection.h>
#include <ChatModel.h>

namespace Ui {
  class WidgetChat;
}

namespace GUI
{
   class WidgetChat : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetChat(CoreConnection& coreConnection, ChatModel& chatModel, QWidget *parent = 0);
      ~WidgetChat();

   private slots:
      void sendMessage();
      void newRows();

   private:
      Ui::WidgetChat *ui;

      CoreConnection& coreConnection;
   };

   class TblChatDelegate : public QItemDelegate
   {
      Q_OBJECT
   public:
      virtual void drawFocus(QPainter*, const QStyleOptionViewItem&, const QRect&) const {}
   };
}
#endif
