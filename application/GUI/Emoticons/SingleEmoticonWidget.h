#ifndef GUI_SINGLEEMOTICONWIDGET_H
#define GUI_SINGLEEMOTICONWIDGET_H

#include <QWidget>
#include <QPixmap>
#include <QString>
#include <QStringList>

namespace Ui {
   class SingleEmoticonWidget;
}

namespace GUI
{
   class SingleEmoticonWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit SingleEmoticonWidget(QWidget* parent = 0);
      ~SingleEmoticonWidget();

      void setImage(const QPixmap& image);
      void setSymbols(const QStringList& list);

   private:
      Ui::SingleEmoticonWidget* ui;
   };
}

#endif
