#ifndef GUI_EMOTICONSWIDGET_H
#define GUI_EMOTICONSWIDGET_H

#include <QWidget>

#include <Emoticons/Emoticons.h>

namespace GUI
{
   class EmoticonsWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit EmoticonsWidget(Emoticons& emoticons, QWidget* parent = 0);

   private:
      Emoticons& emoticons;
   };
}

#endif
