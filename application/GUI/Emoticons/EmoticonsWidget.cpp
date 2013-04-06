#include <Emoticons/EmoticonsWidget.h>
using namespace GUI;

#include <QGridLayout>

EmoticonsWidget::EmoticonsWidget(Emoticons& emoticons, QWidget* parent) :
   QWidget(parent),
   emoticons(emoticons)
{
   QGridLayout* layout = new QGridLayout(this);

}
