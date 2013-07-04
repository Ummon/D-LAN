#include <AutoComplete/AutoComplete.h>
#include <ui_AutoComplete.h>
using namespace GUI;

AutoComplete::AutoComplete(QWidget* parent) :
   QWidget(parent),
   ui(new Ui::AutoComplete)
{
   this->ui->setupUi(this);
   this->ui->listView->setModel(&this->model);
}

void AutoComplete::setFilter(const QString& pattern)
{
   this->model.setFilter(pattern);
}
