#ifndef GUI_PROGRESSBAR_H
#define GUI_PROGRESSBAR_H

#include <QProgressBar>
#include <QPaintEvent>

namespace GUI
{
   class ProgressBar : public QProgressBar
   {
      Q_OBJECT
   public:
      explicit ProgressBar(QWidget *parent = 0);

   protected:
      void paintEvent(QPaintEvent* paintEvent);
   };
}

#endif
