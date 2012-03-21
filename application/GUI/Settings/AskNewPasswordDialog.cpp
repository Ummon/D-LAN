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
  
#include <Settings/AskNewPasswordDialog.h>
#include <ui_AskNewPasswordDialog.h>
using namespace GUI;

#include <QMessageBox>

AskNewPasswordDialog::AskNewPasswordDialog(const Common::Hash& oldPassword, QWidget *parent) :
   QDialog(parent),
   ui(new Ui::AskNewPasswordDialog),
   oldPassword(oldPassword)
{
   ui->setupUi(this);

   if (oldPassword.isNull())
   {
      this->ui->lblOldPassword->hide();
      this->ui->txtOldPassword->hide();
   }

   connect(this->ui->buttons, SIGNAL(rejected()), this, SLOT(reject()));
   connect(this->ui->buttons, SIGNAL(accepted()), this, SLOT(ok()));
}

AskNewPasswordDialog::~AskNewPasswordDialog()
{
   delete this->ui;
}

Common::Hash AskNewPasswordDialog::getNewPassword() const
{
   return Common::Hasher::hashWithSalt(this->ui->txtNewPassword->text());
}

Hash AskNewPasswordDialog::getOldPassword() const
{
   if (this->oldPassword.isNull())
      return Common::Hash();
   return Common::Hasher::hashWithSalt(this->ui->txtOldPassword->text());
}

void AskNewPasswordDialog::ok()
{
   if (this->ui->txtNewPassword->text() != this->ui->txtNewPasswordRepeated->text())
   {
      QMessageBox::information(this, "Error", "Ths passwords aren't the same");
      return;
   }
   else if (this->ui->txtNewPassword->text().isEmpty())
   {
      QMessageBox::information(this, "Error", "The password can't be empty");
      return;
   }
   else if (this->ui->txtNewPassword->text().contains(QRegExp("\\s")))
   {
      QMessageBox::information(this, "Error", "The password can't contain one or more whitespace");
      return;
   }
   else if (!this->oldPassword.isNull() && this->oldPassword != Common::Hasher::hash(Common::Hasher::hashWithSalt(this->ui->txtOldPassword->text())))
   {
      QMessageBox::information(this, "Error", "The old password doesn't match");
      return;
   }
   else
   {
      this->accept();
   }
}
