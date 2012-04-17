/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <TabButtons.h>
using namespace GUI;

/**
  * @class GUI::TabButton
  *
  * Inspired from 'qtabbar_p.h'.
  */

TabButton::TabButton(QWidget* parent) :
   QAbstractButton(parent)
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

    const QTabBar* tb = nullptr;
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

TabCloseButton::TabCloseButton(QWidget* widget, QWidget* parent) :
   TabButton(parent), widget(widget)
{
   connect(this, SIGNAL(clicked()), this, SLOT(buttonClicked()));
   this->setToolTipTranslate();
}

void TabCloseButton::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->setToolTipTranslate();
   else
      TabButton::changeEvent(event);
}

void TabCloseButton::drawPrimitive(const QStyleOption& opt, QPainter& p)
{
   this->style()->drawPrimitive(QStyle::PE_IndicatorTabClose, &opt, &p, this);
}

void TabCloseButton::buttonClicked()
{
   emit clicked(this->widget);

   // Delete the widget added to the tabBar with 'QTabBar::setTabButton(..)'
   // Why the QTabBar do not delete the widget set by 'setTabButton' when the tab is closed!?
   QObject* widgetInTabBar = this;
   while (!dynamic_cast<QTabBar*>(widgetInTabBar->parent()))
      widgetInTabBar = widgetInTabBar->parent();
   delete widgetInTabBar;
}

void TabCloseButton::setToolTipTranslate()
{
   this->setToolTip(tr("Close Tab"));
}

/////

TabRefreshButton::TabRefreshButton(QWidget* parent) :
   TabButton(parent)
{
   this->icon.addPixmap(QPixmap(":/icons/ressources/refresh.png"), QIcon::Normal, QIcon::Off);
   this->icon.addPixmap(QPixmap(":/icons/ressources/refresh-down.png"), QIcon::Normal, QIcon::On);
   this->icon.addPixmap(QPixmap(":/icons/ressources/refresh-hover.png"), QIcon::Active, QIcon::Off);
   this->setToolTipTranslate();
}

void TabRefreshButton::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->setToolTipTranslate();
   else
      TabButton::changeEvent(event);
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

void TabRefreshButton::setToolTipTranslate()
{
   this->setToolTip(tr("Refresh"));
}
