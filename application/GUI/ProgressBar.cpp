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
  
#include <ProgressBar.h>
using namespace GUI;

#include <QApplication>
#include <QStylePainter>

/**
  * @class GUI::ProgressBar
  *
  * With the default format, print the percentage with two decimals after the point (the value is in hundredths of a
  * percent). Its text is readable over the progress, see 'draw(..)'.
  */

ProgressBar::ProgressBar(QWidget *parent) :
   QProgressBar(parent)
{
}

void ProgressBar::paintEvent(QPaintEvent* paintEvent)
{
   QStylePainter paint(this);
   QStyleOptionProgressBar opt;
   initStyleOption(&opt);
   if (this->format() == QStringLiteral("%p%"))
      opt.text = QStringLiteral("%1%").arg(static_cast<double>(this->value()) / 100);
   ProgressBar::draw(this->style(), &paint, opt, this);
}

/**
  * Draws a progress bar as 'QStyle::CE_ProgressBar' does. With a style sheet, Qt draws the text in a single color, which
  * can be unreadable over the progress (black over the dark blue blocks of the style 'retro'): the text is then drawn
  * as the Fusion style does, with the highlighted text color over the progress.
  */
void ProgressBar::draw(QStyle* style, QPainter* painter, const QStyleOptionProgressBar& option, const QWidget* widget)
{
   const qint64 range = qint64(option.maximum) - option.minimum;
   if (qApp->styleSheet().isEmpty() || !option.textVisible || option.text.isEmpty() || !(option.state & QStyle::State_Horizontal) || range <= 0)
   {
      style->drawControl(QStyle::CE_ProgressBar, &option, painter, widget);
      return;
   }

   QStyleOptionProgressBar subOption(option);
   subOption.rect = style->subElementRect(QStyle::SE_ProgressBarGroove, &option, widget);
   style->drawControl(QStyle::CE_ProgressBarGroove, &subOption, painter, widget);
   subOption.rect = style->subElementRect(QStyle::SE_ProgressBarContents, &option, widget);
   style->drawControl(QStyle::CE_ProgressBarContents, &subOption, painter, widget);

   const QRect contents = subOption.rect;
   const int progress = qBound(qint64(0), qint64(option.progress) - option.minimum, range);
   const int filledWidth = int(contents.width() * progress / range);
   const bool fromRight = (option.direction == Qt::RightToLeft) != option.invertedAppearance;
   const QRect filled(fromRight ? contents.right() - filledWidth + 1 : contents.left(), contents.top(), filledWidth, contents.height());

   const QRect label = style->subElementRect(QStyle::SE_ProgressBarLabel, &option, widget);
   const int flags = int(option.textAlignment) | Qt::TextSingleLine;
   const bool enabled = option.state & QStyle::State_Enabled;
   painter->save();
   painter->setClipRegion(QRegion(label).subtracted(filled));
   style->drawItemText(painter, label, flags, option.palette, enabled, option.text, QPalette::Text);
   painter->setClipRect(label.intersected(filled));
   style->drawItemText(painter, label, flags, option.palette, enabled, option.text, QPalette::HighlightedText);
   painter->restore();
}
