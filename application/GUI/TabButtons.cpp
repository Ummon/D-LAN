#include <TabButtons.h>
using namespace GUI;


TabButton::TabButton(QWidget* parent)
   : QAbstractButton(parent)
{
   this->setFocusPolicy(Qt::NoFocus);
   this->resize(this->sizeHint());
}

QSize TabButton::sizeHint() const
{
    this->ensurePolished();
    int width = this->style()->pixelMetric(QStyle::PM_TabCloseIndicatorWidth, 0, this);
    int height = this->style()->pixelMetric(QStyle::PM_TabCloseIndicatorHeight, 0, this);
    return QSize(width, height);
}

QSize TabButton::minimumSizeHint() const
{
   return this->sizeHint();
}

void TabButton::enterEvent(QEvent *event)
{
   if (this->isEnabled())
       this->update();
   QAbstractButton::enterEvent(event);
}

void TabButton::leaveEvent(QEvent *event)
{
    if (this->isEnabled())
        this->update();
    QAbstractButton::leaveEvent(event);
}

void TabButton::paintEvent(QPaintEvent* pe)
{
    QPainter p(this);
    QStyleOption opt;
    opt.init(this);
    opt.state |= QStyle::State_AutoRaise;
    if (isEnabled() && underMouse() && !isChecked() && !isDown())
        opt.state |= QStyle::State_Raised;
    if (isChecked())
        opt.state |= QStyle::State_On;
    if (isDown())
        opt.state |= QStyle::State_Sunken;

    const QTabBar* tb = 0;
    QObject* current = this;

    while(current)
    {
       tb = qobject_cast<const QTabBar*>(current->parent());
       if (tb)
          break;
       current = current->parent();
    }

    if (tb)
    {
        int index = tb->currentIndex();
        QTabBar::ButtonPosition position = (QTabBar::ButtonPosition)style()->styleHint(QStyle::SH_TabBar_CloseButtonPosition, 0, tb);
        if (tb->tabButton(index, position) == current)
            opt.state |= QStyle::State_Selected;
    }

    this->drawPrimitive(opt, p);
}

/////

TabCloseButton::TabCloseButton(QWidget* widget, QWidget* parent)
   : TabButton(parent), widget(widget)
{
   this->setToolTip(tr("Close Tab"));
   connect(this, SIGNAL(clicked()), this, SLOT(buttonClicked()));
}

void TabCloseButton::buttonClicked()
{
   emit clicked(this->widget);
}

void TabCloseButton::drawPrimitive(const QStyleOption& opt, QPainter& p)
{
   this->style()->drawPrimitive(QStyle::PE_IndicatorTabClose, &opt, &p, this);
}

/////

TabRefreshButton::TabRefreshButton(QWidget* parent)
   : TabButton(parent)
{
   this->icon.addPixmap(QPixmap(":/icons/ressources/refresh.png"), QIcon::Normal, QIcon::Off);
   this->icon.addPixmap(QPixmap(":/icons/ressources/refresh-down.png"), QIcon::Normal, QIcon::On);
   this->icon.addPixmap(QPixmap(":/icons/ressources/refresh-hover.png"), QIcon::Active, QIcon::Off);
}

void TabRefreshButton::drawPrimitive(const QStyleOption& opt, QPainter& p)
{
   QIcon::Mode mode = opt.state & QStyle::State_Enabled ? (opt.state & QStyle::State_Raised ? QIcon::Active : QIcon::Normal) : QIcon::Disabled;

   if (!(opt.state & QStyle::State_Raised) && !(opt.state & QStyle::State_Sunken) && !(opt.state & QStyle::State_Selected))
      mode = QIcon::Disabled;

   QIcon::State state = opt.state & QStyle::State_Sunken ? QIcon::On : QIcon::Off;

   QPixmap pixmap = this->icon.pixmap(16, mode, state);
   style()->proxy()->drawItemPixmap(&p, opt.rect, Qt::AlignCenter, pixmap);
}
