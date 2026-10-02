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
  
#include <MainWindow.h>
using namespace PasswordHasher;

#include <QMessageBox>
#include <QStringBuilder>
#include <QRegularExpression>

#include <Protos/core_settings.pb.h>

#include <Common/Hash.h>
#include <Common/Global.h>
#include <Common/Constants.h>
#include <Common/Settings.h>
#include <Common/PersistentData.h>

#include <ui_MainWindow.h>

MainWindow::MainWindow(QWidget *parent) :
   QMainWindow(parent),
   CORE_SETTINGS_PATH_CURRENT_USER(Common::Global::getDataFolder(Common::Global::DataFolderType::ROAMING, false)),
   CORE_SETTINGS_PATH_SYSTEM_USER(Common::Global::getDataServiceFolder(Common::Global::DataFolderType::ROAMING)),
   ui(new Ui::MainWindow)
{
   ui->setupUi(this);
   this->setButtonText();
   this->computeHash();

   connect(this->ui->txtPass1, SIGNAL(textChanged(const QString&)), this, SLOT(computeHash()));
   connect(this->ui->txtPass2, SIGNAL(textChanged(const QString&)), this, SLOT(computeHash()));

   connect(this->ui->butSaveCurrentUser, SIGNAL(clicked()), this, SLOT(savePasswordToCurrentUser()));
   connect(this->ui->butSaveSystemUser, SIGNAL(clicked()), this, SLOT(savePasswordToSystemUser()));

   this->ui->lblInstructions->setText(this->ui->lblInstructions->text().replace("{settings_path_current_user}", CORE_SETTINGS_PATH_CURRENT_USER + '/' + Common::Constants::CORE_SETTINGS_FILENAME));
   this->ui->lblInstructions->setText(this->ui->lblInstructions->text().replace("{settings_path_system_user}", CORE_SETTINGS_PATH_SYSTEM_USER + '/' + Common::Constants::CORE_SETTINGS_FILENAME));

   SETTINGS.setFilename(Common::Constants::CORE_SETTINGS_FILENAME);
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());
}

MainWindow::~MainWindow()
{
   delete ui;
}

void MainWindow::computeHash()
{
   QString error = this->checkPasswords();
   if (!error.isNull())
   {
      this->ui->txtResult->setText(error);
   }
   else
   {
      try
      {
         this->password = Common::SaltedPassword::create(this->ui->txtPass1->text());
         this->ui->txtResult->setText("\"remote_password\": \"" % this->password.toStr() % "\"\n");
      }
      catch (const QString& error)
      {
         this->password = Common::SaltedPassword();
         this->ui->txtResult->setText(error);
      }
   }
}

void MainWindow::savePasswordToCurrentUser()
{
   this->savePassword(CORE_SETTINGS_PATH_CURRENT_USER);
}

void MainWindow::savePasswordToSystemUser()
{
   this->savePassword(CORE_SETTINGS_PATH_SYSTEM_USER);
}

void MainWindow::savePassword(const QString& directory)
{
   QString error = this->checkPasswords();

   if (!error.isNull())
   {
      QMessageBox::warning(this, "Password not saved", error);
   }
   else if (this->password.isNull())
   {
      QMessageBox::warning(this, "Password not saved", this->ui->txtResult->toPlainText());
   }
   else
   {
      SETTINGS.load();
      SETTINGS.set("remote_password", this->password.toStr());

      if (!SETTINGS.saveToACustomDirectory(directory))
         QMessageBox::warning(this, "Error", "The settings file could not be saved.");
      else
         QMessageBox::information(this, "Password saved", "Password has been saved.");
   }
}

void MainWindow::setButtonText()
{
   this->ui->butSaveCurrentUser->setText(QString("Save result to \"%1\"").arg(CORE_SETTINGS_PATH_CURRENT_USER + '/' + Common::Constants::CORE_SETTINGS_FILENAME));
   this->ui->butSaveSystemUser->setText(QString("Save result to \"%1\"").arg(CORE_SETTINGS_PATH_SYSTEM_USER + '/' + Common::Constants::CORE_SETTINGS_FILENAME));
}

/**
  * Checks that the passwords are not empty, are equal and do not contain any whitespace.
  * @return An error message if there is an error or a null string if everything is fine.
  */
QString MainWindow::checkPasswords() const
{
   QRegularExpression spaces("\\s");

   if (this->ui->txtPass1->text() != this->ui->txtPass2->text())
      return QString("Error: the passwords do not match");
   else if (this->ui->txtPass1->text().isEmpty())
      return QString("Error: the password is empty");
   else if (spaces.match(this->ui->txtPass1->text()).hasMatch())
      return QString("Error: the password must not contain whitespace");
   else
      return QString();
}
