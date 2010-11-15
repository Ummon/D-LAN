#include <DialogAbout.h>
#include <ui_DialogAbout.h>
using namespace GUI;

#include<QDateTime>

#include <Common/Version.h>

DialogAbout::DialogAbout(QWidget *parent)
   : QDialog(parent), ui(new Ui::DialogAbout)
{
   this->ui->setupUi(this);

   QDateTime buildTime = QDateTime::fromString(BUILD_TIME, "yyyy-MM-dd_hh-mm");

   this->ui->lblTitle->setText(QString("%1 %2 %3").arg(this->ui->lblTitle->text()).arg(VERSION).arg(VERSION_TAG));
   this->ui->lblBuiltOn->setText(QString("%1 %2 UTC").arg(this->ui->lblBuiltOn->text()).arg(buildTime.toString()));
   this->ui->lblFromRevision->setText(QString("%1 %2").arg(this->ui->lblFromRevision->text()).arg(GIT_VERSION));
}

DialogAbout::~DialogAbout()
{
   delete this->ui;
}
