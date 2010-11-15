#include <StatusBar.h>
#include <ui_StatusBar.h>
using namespace GUI;

#include <Common/Global.h>

#include <DialogAbout.h>

StatusBar::StatusBar(CoreConnection& coreConnection, QWidget *parent)
   : QWidget(parent), ui(new Ui::StatusBar), coreConnection(coreConnection)
{
   this->ui->setupUi(this);
   this->coreDisconnected();

   connect(&coreConnection, SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(&coreConnection, SIGNAL(coreConnected()), this, SLOT(coreConnected()));
   connect(&coreConnection, SIGNAL(coreDisconnected()), this, SLOT(coreDisconnected()));

   connect(this->ui->butHelp, SIGNAL(clicked()), this, SLOT(showAbout()));
}

StatusBar::~StatusBar()
{
   delete this->ui;
}

void StatusBar::coreConnected()
{
   this->ui->lblCoreStatus->setText("Connected");
}

void StatusBar::coreDisconnected()
{
   this->ui->lblCoreStatus->setText("Disconnected");
}

void StatusBar::newState(const Protos::GUI::State& state)
{
   this->ui->lblDownloadRate->setText(Common::Global::formatByteSize(state.stats().download_rate()).append("/s"));
   this->ui->lblUploadRate->setText(Common::Global::formatByteSize(state.stats().upload_rate()).append("/s"));

   qint64 totalSharing = 0;
   for (int i = 0; i < state.peer_size(); i++)
      totalSharing += state.peer(i).sharing_amount();
   totalSharing += state.settings().myself().sharing_amount();

   this->ui->lblTotalSharing->setText(Common::Global::formatByteSize(totalSharing));
}

void StatusBar::showAbout()
{
   DialogAbout about;
   about.exec();
}
