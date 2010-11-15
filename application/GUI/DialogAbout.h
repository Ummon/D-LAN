#ifndef COMMON_DIALOGABOUT_H
#define COMMON_DIALOGABOUT_H

#include <QDialog>

namespace Ui {
   class DialogAbout;
}

namespace GUI
{
   class DialogAbout : public QDialog
   {
      Q_OBJECT
   public:
      explicit DialogAbout(QWidget *parent = 0);
      ~DialogAbout();

   private:
      Ui::DialogAbout *ui;
   };
}

#endif
