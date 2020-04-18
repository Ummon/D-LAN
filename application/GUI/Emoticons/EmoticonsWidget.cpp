/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
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

   if (!this->themeButtons.isEmpty())
      this->themeButtons.first()->setChecked(true); // Is not found.
}

void EmoticonsWidget::emoticonClicked()
{
   SingleEmoticonWidget* emoticonWidget = dynamic_cast<SingleEmoticonWidget*>(this->sender());
   emit emoticonChosen(emoticonWidget->getTheme(), emoticonWidget->getEmoticonName());
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
