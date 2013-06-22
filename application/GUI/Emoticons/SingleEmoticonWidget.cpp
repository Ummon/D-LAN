#include <Emoticons/SingleEmoticonWidget.h>
#include <ui_SingleEmoticonWidget.h>
using namespace GUI;

/**
  * @class GUI::SingleEmoticonWidget
  * A widget which shows an emoticon image and its textual representions like ":)", ":-)", etc..
  */

SingleEmoticonWidget::SingleEmoticonWidget(QWidget *parent) :
   QWidget(parent),
   ui(new Ui::SingleEmoticonWidget)
{
   this->ui->setupUi(this);
}

SingleEmoticonWidget::~SingleEmoticonWidget()
{
   delete this->ui;
}

void SingleEmoticonWidget::setImage(const QPixmap& image)
{
   this->ui->lblEmoticonImage->setPixmap(image);
}

void SingleEmoticonWidget::setSymbols(const QStringList& list)
{
   QString symbols;
   foreach (QString symbol, list)
      symbols.append(symbol).append(" ");

   this->ui->lblEmoticonSymbols->setText(symbols);
}
