#include "WidgetChat.h"
#include "ui_WidgetChat.h"
using namespace GUI;

WidgetChat::WidgetChat(QWidget *parent) :
   QWidget(parent),
   ui(new Ui::WidgetChat)
{
   ui->setupUi(this);
}

WidgetChat::~WidgetChat()
{
   delete ui;
}
