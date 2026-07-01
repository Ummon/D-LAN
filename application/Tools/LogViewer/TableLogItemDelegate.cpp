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
  
#include <TableLogItemDelegate.h>

#include <QApplication>
#include <QPainter>
#include <QAbstractTextDocumentLayout>

#include <Common/LogManager/IEntry.h>

#include <TableLogModel.h>

/**
  * @class TableLogItemDelegate
  *
  * Override the paint method for table log items and
  * draw them in function of their severity.
  */


const QString TableLogItemDelegate::OPEN_HTML_FOUND_TERM("<span style=\"font-weight: bold;color: #FFFF00;background-color: #21218B;\">");
const QString TableLogItemDelegate::CLOSE_HTML_FOUND_TERM("</span>");

TableLogItemDelegate::TableLogItemDelegate(QObject* parent) :
   QStyledItemDelegate(parent)
{
}

void TableLogItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const TableLogModel* model = static_cast<const TableLogModel*>(index.model());

   QStyleOptionViewItem styleOption = option;
   this->initStyleOption(&styleOption, index); // Pulls in selection state, etc.
   QStyle* style = styleOption.widget ? styleOption.widget->style() : QApplication::style();

   QTextDocument doc;
   this->configureDoc(doc, option, index);

   // Draw the styled background/selection but suppress the default text.
   styleOption.text.clear();
   style->drawControl(QStyle::CE_ItemViewItem, &styleOption, painter, styleOption.widget);

   painter->save();

   if (index.column() == TableLogModel::SEVERITY)
   {
      switch (model->getSeverity(index.row()))
      {
      case LM::SV_END_USER :
         painter->fillRect(styleOption.rect, QColor(41, 33, 53));
         break;
      case LM::SV_WARNING :
         painter->fillRect(styleOption.rect, QColor(0, 47, 28));
         break;
      case LM::SV_ERROR :
         painter->fillRect(styleOption.rect, QColor(200, 0, 0));
         // newOption.palette.setColor(QPalette::Text, QColor(255, 255, 255));
         break;
      case LM::SV_FATAL_ERROR :
         painter->fillRect(styleOption.rect, QColor(50, 0, 0));
         // newOption.palette.setColor(QPalette::Text, QColor(255, 255, 0));
         break;
      // No special color for these cases.
      case LM::SV_DEBUG :
      case LM::SV_UNKNOWN :
      default:;
      }
   }

   // Use the correct text colour for the selected/normal state.
   QAbstractTextDocumentLayout::PaintContext ctx;
   if (styleOption.state & QStyle::State_Selected)
      ctx.palette.setColor(QPalette::Text, styleOption.palette.color(QPalette::Active, QPalette::HighlightedText));

   QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &styleOption, styleOption.widget);
   painter->translate(textRect.topLeft());
   painter->setClipRect(textRect.translated(-textRect.topLeft()));

   ctx.clip = QRectF(0, 0, textRect.width(), textRect.height());
   doc.documentLayout()->draw(painter, ctx);

   painter->restore();
}

QSize TableLogItemDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const auto& i = this->sizesCache.find(index);
   if (i != this->sizesCache.constEnd())
      return i.value();

   QStyleOptionViewItem opt = option;
   this->initStyleOption(&opt, index);

   QTextDocument doc;
   this->configureDoc(doc, option, index);

   QSize size(int(doc.idealWidth()), int(doc.size().height()));

   this->sizesCache.insert(index, size);

   return size;
}

void TableLogItemDelegate::resetSizesCache()
{
   this->sizesCache.clear();
}

void TableLogItemDelegate::configureDoc(QTextDocument& doc, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const TableLogModel* model = static_cast<const TableLogModel*>(index.model());

   QStyleOptionViewItem styleOption = option;
   this->initStyleOption(&styleOption, index); // Pulls in selection state, etc.

   QTextOption textOption = doc.defaultTextOption();
   textOption.setWrapMode(QTextOption::NoWrap);
   doc.setDefaultTextOption(textOption);

   if (index.column() == TableLogModel::MESSAGE)
   {
      QString message = styleOption.text;
      if (model->inSearchResult(index))
      {
         const QString& term = model->currentSearchTerm();
         int i = 0;
         while ((i = message.indexOf(term, i, Qt::CaseInsensitive)) != -1)
         {
            message.insert(i, OPEN_HTML_FOUND_TERM);
            i += term.size() + OPEN_HTML_FOUND_TERM.size();
            message.insert(i, CLOSE_HTML_FOUND_TERM);
            i += CLOSE_HTML_FOUND_TERM.size();
         }
         // message.replace(QChar::LineSeparator, "<br>");
         // message.replace("\n", "<br>");
      }
      doc.setHtml(message);
   }
   else
   {
      doc.setPlainText(styleOption.text);
   }

   doc.setDefaultFont(styleOption.font);
   doc.setTextWidth(styleOption.rect.width());
}
