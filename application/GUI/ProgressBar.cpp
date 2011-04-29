#include <ProgressBar.h>
using namespace GUI;

#include <QStylePainter>
#include <QStyleOptionProgressBarV2>

/**
  * @class GUI::ProgressBar
  *
  * Print the percent with two decimals after the point.
  */

ProgressBar::ProgressBar(QWidget *parent) :
   QProgressBar(parent)
{
}

void ProgressBar::paintEvent(QPaintEvent* paintEvent)
{
   QStylePainter paint(this);
   QStyleOptionProgressBarV2 opt;
   initStyleOption(&opt);
   opt.text = QString("%1%").arg(static_cast<double>(this->value()) / 100);
   paint.drawControl(QStyle::CE_ProgressBar, opt);
}
