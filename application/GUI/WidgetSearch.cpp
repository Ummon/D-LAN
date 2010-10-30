#include "WidgetSearch.h"
#include "ui_WidgetSearch.h"
using namespace GUI;

WidgetSearch::WidgetSearch(CoreConnection& coreConnection, PeerListModel& peerListModel, const QString& terms, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetSearch), coreConnection(coreConnection), searchModel(coreConnection, peerListModel)
{
    this->ui->setupUi(this);

    connect(&this->searchModel, SIGNAL(progress(int)), this->ui->prgSearch, SLOT(setValue(int)));

    this->ui->treeView->setModel(&this->searchModel);

    this->searchModel.search(terms);

    this->setWindowTitle(QString("\"%1\"").arg(terms));
}

WidgetSearch::~WidgetSearch()
{
    delete this->ui;
}
