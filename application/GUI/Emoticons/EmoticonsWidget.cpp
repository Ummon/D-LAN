#include <Emoticons/EmoticonsWidget.h>
using namespace GUI;

#include <QGridLayout>
#include <QRadioButton>

/**
  * @class GUI::EmoticonsWidget
  *
  */

EmoticonsWidget::EmoticonsWidget(Emoticons& emoticons, QWidget* parent) :
   QWidget(parent),
   emoticons(emoticons)
{
   QGridLayout* layout = new QGridLayout(this);

   int row = 0;
   foreach (QString theme, this->emoticons.getThemes())
   {
      QRadioButton* radio = new QRadioButton(this);
      radio->setText(theme);
      layout->addWidget(radio, row, 0, 1, -1);
      row++;

      /*foreach (QString smileName, this->emoticons.getSmileNames(theme))
      {

      }*/
   }
}
