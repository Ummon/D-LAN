#include "TooglableList.h"

#include <QPushButton>

#include "ui_TooglableList.h"

TooglableList::TooglableList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::TooglableList)
{
   this->ui->setupUi(this);
}

TooglableList::~TooglableList()
{
    delete this->ui;
}

void TooglableList::setList(const QStringList& list)
{
   foreach (QString item, list)
   {
      QLayout* lay = this->ui->widContent->layout();
      QPushButton* but = new QPushButton(this->ui->widContent);
      but->sizePolicy().setHorizontalPolicy(QSizePolicy::MinimumExpanding);
      but->setText(item);
      but->setCheckable(true);
      connect(but, SIGNAL(toggled(bool)), this, SIGNAL(stateChanged()));
      lay->addWidget(but);
   }
}

QList<ToogleState> TooglableList::getList()
{
   QList<ToogleState> list;
   foreach (QObject* object, this->ui->widContent->children())
   {
      if (QPushButton* button = dynamic_cast<QPushButton*>(object))
         list << ToogleState(button->isChecked(), button->text());
   }
   return list;
}
