#include <WidgetBrowse.h>
#include <ui_WidgetBrowse.h>
using namespace GUI;

WidgetBrowse::WidgetBrowse(CoreConnection& coreConnection, PeerListModel& model, const Common::Hash& peerID, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetBrowse), coreConnection(coreConnection), peerListModel(model), peerID(peerID), browseModel(coreConnection, peerID)
{
    this->ui->setupUi(this);

    this->ui->treeView->setModel(&this->browseModel);


   this->setWindowTitle(QString("[%1]").arg(this->peerListModel.getNick(this->peerID)));
}

WidgetBrowse::~WidgetBrowse()
{
    delete this->ui;
}

Common::Hash WidgetBrowse::getPeerID() const
{
   return this->peerID;
}
