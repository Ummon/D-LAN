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

      void setTheme(const QString& theme);
      const QString& getTheme() const;

      void setEmoticonName(const QString& emoticonName);
      const QString& getEmoticonName() const;

   signals:
      void clicked();

   protected:
      void leaveEvent(QEvent* event) override;
      void enterEvent(QEvent* event) override;
      void	mousePressEvent(QMouseEvent* event) override;

   private:
      Ui::SingleEmoticonWidget* ui;

      QString themeName;
      QString emoticonName;
   };
}

#endif
