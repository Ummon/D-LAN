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

#include <Emoticons/SingleEmoticonWidget.h>
#include <ui_SingleEmoticonWidget.h>
using namespace GUI;

/**
  * @class GUI::SingleEmoticonWidget
  * A widget which shows an emoticon image and its textual representations like ":)", ":-)", etc . . .
  */

SingleEmoticonWidget::SingleEmoticonWidget(QWidget *parent) :
   QWidget(parent),
   ui(new Ui::SingleEmoticonWidget)
{
   this->ui->setupUi(this);
}

SingleEmoticonWidget::~SingleEmoticonWidget()
{
   delete this->ui;
}

void SingleEmoticonWidget::setImage(const QPixmap& image)
{
   this->ui->lblEmoticonImage->setPixmap(image);
}

void SingleEmoticonWidget::setSymbols(const QStringList& list)
{
   this->ui->lblEmoticonImage->setToolTip(list.join(" "));
}

void SingleEmoticonWidget::setTheme(const QString& theme)
{
   this->themeName = theme;
}

const QString& SingleEmoticonWidget::getTheme() const
{
   return this->themeName;
}

void SingleEmoticonWidget::setEmoticonName(const QString& emoticonName)
{
   this->emoticonName = emoticonName;
}

const QString& SingleEmoticonWidget::getEmoticonName() const
{
   return this->emoticonName;
}

void SingleEmoticonWidget::leaveEvent(QEvent*)
{
   this->setBackgroundRole(QPalette::Window);
}

void SingleEmoticonWidget::enterEvent(QEvent*)
{
   this->setBackgroundRole(QPalette::Highlight);
}

void	SingleEmoticonWidget::mousePressEvent(QMouseEvent*)
{
   emit clicked();
}
