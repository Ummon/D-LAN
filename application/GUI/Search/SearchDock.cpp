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
  
#include <Search/SearchDock.h>
#include <ui_SearchDock.h>
using namespace GUI;

#include <QKeyEvent>
#include <QIntValidator>

#include <Common/ProtoHelper.h>
#include <Common/Settings.h>
#include <Common/Constants.h>

SearchDock::SearchDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QDockWidget(parent),
   ui(new Ui::SearchDock),
   coreConnection(coreConnection)
{
   this->ui->setupUi(this);

#ifdef Q_OS_DARWIN
   this->ui->butSearch->setMaximumWidth(24);
   this->ui->butSearchOwnFiles->setMaximumWidth(24);
#endif

   connect(this->ui->butSearch, SIGNAL(clicked()), this, SLOT(search()));
   this->ui->txtSearch->installEventFilter(this); // the signal 'returnPressed()' doesn't contain the key modifier information (shift = search among our files), we have to use a event filter.

   this->ui->txtMinSize->setValidator(new QIntValidator(this));
   this->ui->txtMaxSize->setValidator(new QIntValidator(this));

   for (int i = 0; i < 5; i++)
   {
      this->ui->cmbMinSize->addItem(Common::Constants::BINARY_PREFIXS[i]);
      this->ui->cmbMaxSize->addItem(Common::Constants::BINARY_PREFIXS[i]);
   }

   this->loadSettings();

   connect(this->ui->txtMinSize, SIGNAL(textChanged(QString)), this, SLOT(saveSettings()));
   connect(this->ui->txtMaxSize, SIGNAL(textChanged(QString)), this, SLOT(saveSettings()));

   connect(this->ui->cmbMinSize, SIGNAL(currentIndexChanged(int)), this, SLOT(saveSettings()));
   connect(this->ui->cmbMaxSize, SIGNAL(currentIndexChanged(int)), this, SLOT(saveSettings()));

   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreDisconnected(false); // Initial state.
}

SearchDock::~SearchDock()
{
   delete this->ui;
}

void SearchDock::setFocusToLineEdit()
{
   this->ui->txtSearch->setFocus();
   this->ui->txtSearch->selectAll();
}

void SearchDock::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);

   QDockWidget::changeEvent(event);
}

bool SearchDock::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->ui->txtSearch && event->type() == QEvent::KeyPress && static_cast<QKeyEvent*>(event)->key() == Qt::Key_Return)
      this->search();

   return QDockWidget::eventFilter(obj, event);
}

void SearchDock::coreConnected()
{
   this->ui->txtSearch->setDisabled(false);
   this->ui->butSearch->setDisabled(false);
}

void SearchDock::coreDisconnected(bool force)
{
   this->ui->txtSearch->setDisabled(true);
   this->ui->butSearch->setDisabled(true);
}

void SearchDock::search()
{
   this->ui->txtSearch->setText(this->ui->txtSearch->text().trimmed());

   Protos::Common::FindPattern pattern;

   Common::ProtoHelper::setStr(pattern, &Protos::Common::FindPattern::set_pattern, this->ui->txtSearch->text());

   //pattern.set_min_size(this->ui->

   emit search(pattern);
}

void SearchDock::saveSettings()
{
   SETTINGS.set("search_min_size_value", this->ui->txtMinSize->text().toUInt());
   SETTINGS.set("search_max_size_value", this->ui->txtMaxSize->text().toUInt());

   SETTINGS.set("search_min_size_unit", (quint32)(this->ui->cmbMinSize->currentIndex() + 1));
   SETTINGS.set("search_max_size_unit", (quint32)(this->ui->cmbMaxSize->currentIndex() + 1));
}

void SearchDock::loadSettings()
{
   quint32 minSize = SETTINGS.get<quint32>("search_min_size_value");
   quint32 maxSize = SETTINGS.get<quint32>("search_max_size_value");

   this->ui->txtMinSize->setText(minSize == 0 ? QString() : QString::number(minSize));
   this->ui->txtMaxSize->setText(maxSize == 0 ? QString() : QString::number(maxSize));

   this->ui->cmbMinSize->setCurrentIndex(SETTINGS.get<quint32>("search_min_size_unit") - 1);
   this->ui->cmbMaxSize->setCurrentIndex(SETTINGS.get<quint32>("search_max_size_unit") - 1);
}
