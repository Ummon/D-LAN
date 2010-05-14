#include "TooglableList.h"

#include <QPushButton>

#include "ui_TooglableList.h"

TooglableList::TooglableList(QWidget *parent) :
    QWidget(parent),
    disableSignalStateChanged(false),
    ui(new Ui::TooglableList)
{
   this->ui->setupUi(this);
   connect(this->ui->butAll, SIGNAL(clicked()), this, SLOT(checkAll()));
}

TooglableList::~TooglableList()
{
    delete this->ui;
}

/**
  * Define the terms which can be filtered.
  */
void TooglableList::setList(const QStringList& list)
{
   this->clear();
   foreach (QString item, list)
      this->addItem(item);
}

QStringList TooglableList::getList()
{
   QStringList list;
   foreach (QObject* object, this->ui->widContent->children())
   {
      QPushButton* button = dynamic_cast<QPushButton*>(object);
      if (button && button->isChecked())
         list << button->text();
   }
   return list;
}

void TooglableList::addItem(const QString& item)
{
   QLayout* lay = this->ui->widContent->layout();
   TooglableListButton* but = new TooglableListButton(this->ui->widContent);
   but->sizePolicy().setHorizontalPolicy(QSizePolicy::MinimumExpanding);
   but->setText(item);
   but->setCheckable(true);
   but->setChecked(true);
   connect(but, SIGNAL(toggled(bool)), this, SLOT(butToogled(bool)));
   connect(but, SIGNAL(rightClicked()), this, SLOT(butRightClicked()));

   lay->addWidget(but);
}

void TooglableList::checkAll()
{
   this->disableSignalStateChanged = true;
   bool stateHasChanged = false;
   foreach (QObject* object, this->ui->widContent->children())
      if (QPushButton* button = dynamic_cast<QPushButton*>(object))
         if (!button->isChecked())
         {
            stateHasChanged = true;
            button->setChecked(true);
         }
   this->disableSignalStateChanged = false;

   if (stateHasChanged)
      emit stateChanged();
}

void TooglableList::checkOne(QPushButton& but)
{
   this->disableSignalStateChanged = true;
   bool stateHasChanged = false;

   if (!but.isChecked())
   {
      stateHasChanged = true;
      but.setChecked(true);
   }

   foreach (QObject* object, this->ui->widContent->children())
      if (QPushButton* button = dynamic_cast<QPushButton*>(object))
         if (button != &but && button->isChecked())
         {
            stateHasChanged = true;
            button->setChecked(false);
         }

   this->disableSignalStateChanged = false;

   if (stateHasChanged)
      emit stateChanged();
}

/**
  * Check that all button are not unchecked.
  */
void TooglableList::butToogled(bool)
{
   QPushButton* toggledBut = static_cast<QPushButton*>(QWidget::sender());

   foreach (QObject* object, this->ui->widContent->children())
      if (QPushButton* button = dynamic_cast<QPushButton*>(object))
         if (button->isChecked())
         {
            if (!this->disableSignalStateChanged)
               emit stateChanged();
            return;
         }

   toggledBut->setChecked(true);
}

void TooglableList::butRightClicked()
{
   QPushButton* toggledBut = static_cast<QPushButton*>(QWidget::sender());
   this->checkOne(*toggledBut);
}

void TooglableList::clear()
{
   foreach (QObject* object, this->ui->widContent->children())
      if (QPushButton* button = dynamic_cast<QPushButton*>(object))
         delete button;
}

