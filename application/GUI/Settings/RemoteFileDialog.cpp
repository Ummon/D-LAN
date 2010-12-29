#include <Settings/RemoteFileDialog.h>
#include <ui_RemoteFileDialog.h>
using namespace GUI;

RemoteFileDialog::RemoteFileDialog(QWidget *parent)
   : QDialog(parent), ui(new Ui::RemoteFileDialog)
{
   this->ui->setupUi(this);
}

RemoteFileDialog::~RemoteFileDialog()
{
   delete this->ui;
}

void RemoteFileDialog::setText(const QString& text)
{
   this->ui->lblDescription->setText(text);
}

QString RemoteFileDialog::getFolder() const
{
   return this->ui->txtPath->text();
}
