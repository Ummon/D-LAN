#include <Emoticons/EmoticonsWidget.h>
using namespace GUI;

#include <QGridLayout>

#include <Emoticons/SingleEmoticonWidget.h>

/**
  * @class GUI::EmoticonsWidget
  *
  */

EmoticonsWidget::EmoticonsWidget(Emoticons& emoticons, QWidget* parent) :
   QWidget(parent),
   emoticons(emoticons)
{
   const int NUMBER_OF_COLUMNS = 8;
   QGridLayout* layout = new QGridLayout(this);

   int row = 0;
   foreach (QString theme, this->emoticons.getThemes())
   {
      QRadioButton* radio = new QRadioButton(this);
      connect(radio, SIGNAL(toggled(bool)), this, SLOT(themeButtonToggled(bool)));
      this->themeButtons << radio;
      radio->setToolTip(tr("Set as the default theme"));
      radio->setText(theme);
      layout->addWidget(radio, row, 0, 1, -1);
      row++;

      int col = 0;
      foreach (QString smileName, this->emoticons.getSmileNames(theme))
      {
         SingleEmoticonWidget* emoticonWidget = new SingleEmoticonWidget(this);
         connect(emoticonWidget, SIGNAL(clicked()), this, SLOT(emoticonClicked()));
         emoticonWidget->setTheme(theme);
         emoticonWidget->setEmoticonName(smileName);
         emoticonWidget->setSymbols(this->emoticons.getSmileSymbols(theme, smileName));
         emoticonWidget->setImage(this->emoticons.getSmileImage(theme, smileName));
         layout->addWidget(emoticonWidget, row, col, 1, 1);
         if (++col >= NUMBER_OF_COLUMNS)
         {
            col = 0;
            row++;
         }
      }

      if (col != 0)
         row++;
   }
}

void EmoticonsWidget::setDefaultTheme(const QString& theme)
{
   foreach (QRadioButton* radio, this->themeButtons)
      if (radio->text() == theme)
      {
         radio->setChecked(true);
         return;
      }

   this->themeButtons.first()->setChecked(true); // Is not found.
}

void EmoticonsWidget::emoticonClicked()
{
   SingleEmoticonWidget* emoticonWidget = dynamic_cast<SingleEmoticonWidget*>(this->sender());
   emit emoticonChoosen(emoticonWidget->getTheme(), emoticonWidget->getEmoticonName());
}

void EmoticonsWidget::themeButtonToggled(bool checked)
{
   if (checked)
   {
      QRadioButton* sender = dynamic_cast<QRadioButton*>(this->sender());
      emit defaultThemeChanged(sender->text());
   }
}

void EmoticonsWidget::showEvent(QShowEvent*)
{
   this->setDefaultTheme(this->emoticons.getDefaultTheme());
}

void EmoticonsWidget::hideEvent(QHideEvent*)
{
   emit hidden();
}
