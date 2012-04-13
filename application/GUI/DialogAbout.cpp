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
  
#include <DialogAbout.h>
#include <ui_DialogAbout.h>
using namespace GUI;

#include <QPainter>
#include <QDateTime>
#include <QLocale>

#include <Common/Version.h>
#include <Common/Settings.h>

DialogAbout::DialogAbout(QWidget *parent) :
   QDialog(parent), ui(new Ui::DialogAbout)
{
   this->ui->setupUi(this);

   this->setWindowFlags(this->windowFlags() & (~Qt::WindowContextHelpButtonHint));

   QDateTime buildTime = QDateTime::fromString(BUILD_TIME, "yyyy-MM-dd_hh-mm");

   QLocale locale = SETTINGS.get<QLocale>("language");

   this->ui->lblTitle->setText(QString("%1 %2 %3").arg(this->ui->lblTitle->text()).arg(VERSION).arg(VERSION_TAG));
   this->ui->lblBuiltOn->setText(QString("%1 %2").arg(this->ui->lblBuiltOn->text()).arg(locale.toString(buildTime)));
   this->ui->lblFromRevision->setText(QString("%1 %2").arg(this->ui->lblFromRevision->text()).arg(GIT_VERSION));

#ifdef DEBUG
   this->ui->lblTitle->setText(this->ui->lblTitle->text() + " (DEBUG)");
#endif
}

DialogAbout::~DialogAbout()
{
   delete this->ui;
}

/**
  * Draw a nice gradient sky as background.
  */
void DialogAbout::paintEvent(QPaintEvent* event)
{
   QPainter p(this);
   QRadialGradient gradient(QPointF(0, 0), 2, QPointF(0, 0));
   gradient.setCoordinateMode(QGradient::StretchToDeviceMode);
   gradient.setColorAt(0, QColor(24, 36, 48));
   gradient.setColorAt(1, QColor(102, 150, 201));
   QBrush brush(gradient);
   p.fillRect(QRect(0, 0, width(), height()), brush);
}


void DialogAbout::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);
   else
      QWidget::changeEvent(event);
}
