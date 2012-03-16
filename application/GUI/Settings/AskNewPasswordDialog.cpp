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
