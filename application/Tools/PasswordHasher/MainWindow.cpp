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

#include <google/protobuf/text_format.h>

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>

#include <Common/Hash.h>
#include <Common/Global.h>
#include <Common/Constants.h>
#include <Common/Settings.h>
#include <Common/PersistentData.h>

#include <ui_MainWindow.h>

MainWindow::MainWindow(QWidget *parent) :
   QMainWindow(parent),
   CORE_SETTINGS_PATH_CURRENT_USER(Common::Global::getDataFolder(Common::Global::ROAMING, false)),
   CORE_SETTINGS_PATH_SYSTEM_USER(Common::Global::getDataServiceFolder(Common::Global::ROAMING)),
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
      Common::Hash hash = Common::Hasher::hashWithRandomSalt(this->ui->txtPass1->text(), this->salt);

      Protos::Core::Settings settings;
      const google::protobuf::FieldDescriptor* passField = settings.GetDescriptor()->FindFieldByName("remote_password");
      const google::protobuf::FieldDescriptor* saltField = settings.GetDescriptor()->FindFieldByName("salt");

      settings.mutable_remote_password()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      settings.set_salt(this->salt);

      std::string encodedHash;
      std::string encodedSalt;
      google::protobuf::TextFormat::PrintFieldValueToString(settings, passField, -1, &encodedHash);
      google::protobuf::TextFormat::PrintFieldValueToString(settings, saltField, -1, &encodedSalt);

      this->ui->txtResult->setText("remote_password {\n " % QString::fromStdString(encodedHash) % "}\nsalt: " % QString::fromStdString(encodedSalt) % "\n");
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
      QMessageBox::warning(this, "Password cannot be saved", error);
   }
   else
   {
      SETTINGS.load();
      SETTINGS.set("remote_password", Common::Hasher::hashWithSalt(this->ui->txtPass1->text(), this->salt));
      SETTINGS.set("salt", this->salt);

      if (!SETTINGS.saveToACutomDirectory(directory))
         QMessageBox::warning(this, "Error", "Error during saving");
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
  * Checks that password are not empty, are equal and do not contain any whitespace.
  * @return An error message if there is an error or a null string if everything is fine.
  */
QString MainWindow::checkPasswords() const
{
   if (this->ui->txtPass1->text() != this->ui->txtPass2->text())
      return QString("Error: passwords aren't the same");
   else if (this->ui->txtPass1->text().isEmpty())
      return QString("Error: password is empty");
   else if (this->ui->txtPass1->text().contains(QRegExp("\\s")))
      return QString("Error: password contains one or more whitespace");
   else
      return QString();
}
