#ifndef GUI_AUTOCOMPLETE_H
#define GUI_AUTOCOMPLETE_H

#include <QWidget>

namespace Ui {
   class AutoComplete;
}

namespace GUI
{
   class AutoComplete : public QWidget
   {
      Q_OBJECT
   public:
      explicit AutoComplete(QWidget* parent = 0);

   signals:

   public slots:

   private:
      Ui::AutoComplete* ui;

   };
}

#endif
