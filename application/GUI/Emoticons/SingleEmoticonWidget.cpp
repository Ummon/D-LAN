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
   this->ui->lblEmoticonImage->setToolTip(list.join(" "));
}

void SingleEmoticonWidget::setTheme(const QString& theme)
{
   this->themeName = theme;
}

const QString& SingleEmoticonWidget::getTheme() const
{
   return this->themeName;
}

void SingleEmoticonWidget::setEmoticonName(const QString& emoticonName)
{
   this->emoticonName = emoticonName;
}

const QString& SingleEmoticonWidget::getEmoticonName() const
{
   return this->emoticonName;
}

void SingleEmoticonWidget::leaveEvent(QEvent*)
{
   this->setBackgroundRole(QPalette::Background);
}

void SingleEmoticonWidget::enterEvent(QEvent*)
{
   this->setBackgroundRole(QPalette::Highlight);
}

void	SingleEmoticonWidget::mousePressEvent(QMouseEvent*)
{
   emit clicked();
}
