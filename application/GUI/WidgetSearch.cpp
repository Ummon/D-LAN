#include "WidgetSearch.h"
#include "ui_WidgetSearch.h"
using namespace GUI;

WidgetSearch::WidgetSearch(CoreConnection& coreConnection, const QString& term, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetSearch), coreConnection(coreConnection)
{
    ui->setupUi(this);

    this->setWindowTitle(QString("\"%1\"").arg(term));
}

WidgetSearch::~WidgetSearch()
{
    delete ui;
}
