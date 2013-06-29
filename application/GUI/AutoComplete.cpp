#include <AutoComplete.h>
#include <ui_AutoComplete.h>
using namespace GUI;


AutoComplete::AutoComplete(QWidget* parent) :
   QWidget(parent),
   ui(new Ui::AutoComplete)
{
   this->ui->setupUi(this);
}
