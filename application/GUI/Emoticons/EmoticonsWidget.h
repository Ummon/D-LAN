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
  
#pragma once

#include <QWidget>
#include <QRadioButton>

#include <Emoticons/Emoticons.h>

namespace GUI
{
   class EmoticonsWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit EmoticonsWidget(Emoticons& emoticons, QWidget* parent = 0);

   signals:
      void hidden();
      void emoticonChoosen(const QString& theme, const QString& emoticonName);
      void defaultThemeChanged(const QString& theme);

   protected:
      void showEvent(QShowEvent* event) override;
      void hideEvent(QHideEvent* event) override;

   private slots:
      void setDefaultTheme(const QString& theme);
      void emoticonClicked();
      void themeButtonToggled(bool checked);

   private:
      Emoticons& emoticons;
      QList<QRadioButton*> themeButtons;
   };
}
