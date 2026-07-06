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
#include <QStringBuilder>
#include <QModelIndex>
#include <QMainWindow>

#include <Common/ProtoHelper.h>
#include <Common/Settings.h>
#include <Common/Constants.h>

#include <Log.h>

// To activate the possibility to hide the advanced fields.
// There is actually some difficulties to hide these controls.
#define HIDE_BUTTON true

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

   auto sizeValidator = new QIntValidator(this);
   sizeValidator->setBottom(0);
   this->ui->txtMinSize->setValidator(sizeValidator);
   this->ui->txtMaxSize->setValidator(sizeValidator);

   for (int i = 0; i < 5; i++)
   {
      this->ui->cmbMinSize->addItem(Common::Constants::BINARY_PREFIXES[i]);
      this->ui->cmbMaxSize->addItem(Common::Constants::BINARY_PREFIXES[i]);
   }

   this->updateComboTypes();

   this->loadSettings();

   connect(this->ui->butSearch, &QPushButton::clicked, this, qOverload<>(&SearchDock::search));
   connect(this->ui->butClear, &QPushButton::clicked, this, &SearchDock::clear);

   connect(this->ui->cmbType, &QComboBox::currentIndexChanged, this, &SearchDock::saveSettings);

   connect(this->ui->txtMinSize, &QLineEdit::textChanged, this, &SearchDock::saveSettings);
   connect(this->ui->txtMaxSize, &QLineEdit::textChanged, this, &SearchDock::saveSettings);

   connect(this->ui->cmbMinSize, &QComboBox::currentIndexChanged, this, &SearchDock::saveSettings);
   connect(this->ui->cmbMaxSize, &QComboBox::currentIndexChanged, this, &SearchDock::saveSettings);

   connect(this->ui->chkOwnFiles, &QCheckBox::checkStateChanged, this, &SearchDock::saveSettings);

   connect(this->coreConnection.data(), &RCC::ICoreConnection::connected, this, &SearchDock::coreConnected);
   connect(this->coreConnection.data(), &RCC::ICoreConnection::disconnected, this, &SearchDock::coreDisconnected);

   // When re-docked after having been floating, the QMainWindow restores the floating height
   // (a bit too tall). Deferred call: the dock geometry isn't final yet when the signal is emitted.
   connect(this, &QDockWidget::topLevelChanged, this, [this](bool floating) {
      if (!floating)
         QMetaObject::invokeMethod(this, [this]() { this->adjustHeight(); }, Qt::QueuedConnection);
   });

#if not HIDE_BUTTON
   this->ui->butAdvanced->hide();
#else
   connect(this->ui->butAdvanced, &QPushButton::clicked, this, &SearchDock::advancedOptionsVisibility);
#endif

   this->adjustHeight(); // Match the initial visibility state loaded by 'loadSettings()'.

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

   this->updateComboTypes();

   QDockWidget::changeEvent(event);
}

void SearchDock::keyPressEvent(QKeyEvent* event)
{
   if (event->key() == Qt::Key_Return)
      this->search();
   else
      QDockWidget::keyPressEvent(event);
}

void SearchDock::coreConnected()
{
   this->ui->txtSearch->setDisabled(false);
   this->ui->butSearch->setDisabled(false);
}

void SearchDock::coreDisconnected(bool force)
{
   this->ui->butSearch->setDisabled(true);
}

void SearchDock::advancedOptionsVisibility(bool shown)
{
   this->ui->advancedOptions->setVisible(shown);
   this->adjustHeight();
}

/**
  * Pin the dock content to the height of its layout. A docked QDockWidget is sized by the
  * QMainWindow dock layout, so 'resize()' has no effect: the only reliable way to change its
  * height is to constrain it.
  */
void SearchDock::adjustHeight()
{
   QWidget* contents = this->ui->dockWidgetContents;
   contents->layout()->activate(); // Take the new visibility into account right now, otherwise 'sizeHint()' is stale.
   contents->setFixedHeight(contents->sizeHint().height());

   // The QMainWindow may have memorized another height for the dock (for example the one it had
   // while floating, native frame included): explicitly ask to re-apply the constraint.
   if (!this->isFloating())
      if (QMainWindow* mainWindow = qobject_cast<QMainWindow*>(this->parentWidget()))
         mainWindow->resizeDocks({ this }, { contents->height() }, Qt::Vertical);
}

void SearchDock::search()
{
   if (!this->coreConnection->isConnected())
      return;

   this->ui->txtSearch->setText(this->ui->txtSearch->text().trimmed());

   if (
      this->ui->txtSearch->text().isEmpty() &&
      this->currentType().entryType != SearchType::EntryType::FILES_BY_EXTENSION &&
      this->currentMinSize() == 0 &&
      this->currentMaxSize() == 0
   )
      return;

   Protos::Common::FindPattern pattern;
   pattern.set_pattern(this->ui->txtSearch->text().toStdString());

   bool local = false;

   if (this->ui->advancedOptions->isVisible())
   {
      SearchType type = this->currentType();
      if (type.entryType == SearchType::EntryType::FILES_BY_EXTENSION)
      {
         foreach (QString e, Common::KnownExtensions::getExtensions(type.extensionCategory))
            pattern.add_extension_filters(e.toStdString());

          pattern.set_category(Protos::Common::FindPattern::FILE);
      }
      else
      {
         pattern.set_category(static_cast<Protos::Common::FindPattern_Category>(type.entryType));
      }

      pattern.set_min_size(this->currentMinSize());
      pattern.set_max_size(this->currentMaxSize());

      local = this->ui->chkOwnFiles->checkState() == Qt::Checked;
   }

   emit search(pattern, local);
}

void SearchDock::clear()
{
   this->ui->txtSearch->setText("");
   this->ui->cmbType->setCurrentIndex(0);

   this->ui->cmbMinSize->setCurrentIndex(2);
   this->ui->txtMinSize->setText("");

   this->ui->cmbMaxSize->setCurrentIndex(2);
   this->ui->txtMaxSize->setText("");

   this->ui->chkOwnFiles->setChecked(false);
}

void SearchDock::saveSettings()
{
   SETTINGS.set("search_type", static_cast<quint32>(this->ui->cmbType->currentIndex()));

   SETTINGS.set("search_min_size_value", this->ui->txtMinSize->text().toUInt());
   SETTINGS.set("search_max_size_value", this->ui->txtMaxSize->text().toUInt());

   SETTINGS.set("search_min_size_unit", (quint32)(this->ui->cmbMinSize->currentIndex() + 1));
   SETTINGS.set("search_max_size_unit", (quint32)(this->ui->cmbMaxSize->currentIndex() + 1));

   SETTINGS.set("search_local", this->ui->chkOwnFiles->checkState() == Qt::Checked);

#if HIDE_BUTTON
   SETTINGS.set("search_advanced_visible", this->ui->advancedOptions->isVisible());
#endif
}

void SearchDock::loadSettings()
{
   this->ui->cmbType->setCurrentIndex(SETTINGS.get<quint32>("search_type"));

   quint32 minSize = SETTINGS.get<quint32>("search_min_size_value");
   quint32 maxSize = SETTINGS.get<quint32>("search_max_size_value");

   this->ui->txtMinSize->setText(minSize == 0 ? QString() : QString::number(minSize));
   this->ui->txtMaxSize->setText(maxSize == 0 ? QString() : QString::number(maxSize));

   this->ui->cmbMinSize->setCurrentIndex(SETTINGS.get<quint32>("search_min_size_unit") - 1);
   this->ui->cmbMaxSize->setCurrentIndex(SETTINGS.get<quint32>("search_max_size_unit") - 1);

   this->ui->chkOwnFiles->setChecked(SETTINGS.get<bool>("search_local"));

#if HIDE_BUTTON
   const bool SHOW_ADVANCED_OPTIONS = SETTINGS.get<bool>("search_advanced_visible");
   this->ui->advancedOptions->setVisible(SHOW_ADVANCED_OPTIONS);
   this->ui->butAdvanced->setChecked(SHOW_ADVANCED_OPTIONS);
#endif
}

void SearchDock::updateComboTypes()
{
   const int previousCurrentIndex = this->ui->cmbType->currentIndex();

   this->ui->cmbType->clear();

   const QList<SearchType> searchTypes = {
      SearchType::EntryType::ALL,
      SearchType::EntryType::DIRS_ONLY,
      SearchType::EntryType::FILES_ONLY,
      Common::ExtensionCategory::AUDIO,
      Common::ExtensionCategory::VIDEO,
      Common::ExtensionCategory::PICTURE,
      Common::ExtensionCategory::DOCUMENT,
      Common::ExtensionCategory::EXECUTABLE,
      Common::ExtensionCategory::SUBTITLE,
      Common::ExtensionCategory::COMPRESSED,
      Common::ExtensionCategory::MEDIA_ARCHIVE
   };

   foreach (SearchType searchType, searchTypes)
   {
      QVariant v;
      v.setValue(searchType);
      this->ui->cmbType->addItem(SearchUtils::getSearchTypeText(searchType, false), v);
   }

   if (previousCurrentIndex != -1)
      this->ui->cmbType->setCurrentIndex(previousCurrentIndex);
}

SearchType SearchDock::currentType() const
{
   return this->ui->cmbType->itemData(this->ui->cmbType->currentIndex()).value<SearchType>();
}

/**
  * Return the mininum size in bytes. 0 if no size has been defined.
  */
quint64 SearchDock::currentMinSize()
{
   quint64 result = this->ui->txtMinSize->text().toUInt();

   if (result == 0)
      return 0;

   for (int i = 0; i < this->ui->cmbMinSize->currentIndex(); i++)
      result *= 1024;

   return result;
}

/**
  * Return the maximum size in bytes. 0 if no size has been defined.
  */
quint64 SearchDock::currentMaxSize()
{
   quint64 result = this->ui->txtMaxSize->text().toUInt();

   if (result == 0)
      return 0;

   for (int i = 0; i < this->ui->cmbMaxSize->currentIndex(); i++)
      result *= 1024;

   return result;
}
