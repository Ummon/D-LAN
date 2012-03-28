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

AskNewPasswordDialog::AskNewPasswordDialog(bool askOldPassword, QWidget *parent) :
   QDialog(parent),
   ui(new Ui::AskNewPasswordDialog)
{
   ui->setupUi(this);

   if (!askOldPassword)
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

QString AskNewPasswordDialog::getNewPassword() const
{
   return this->ui->txtNewPassword->text();
}

QString AskNewPasswordDialog::getOldPassword() const
{
   return this->ui->txtOldPassword->text();
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
   else if (!this->ui->txtOldPassword->isHidden() && this->ui->txtOldPassword->text().isEmpty())
   {
      QMessageBox::information(this, "Error", "The old password is required");
      return;
   }
   else
   {
      this->accept();
   }
}
