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
#include <QTextCursor>
#include <cmath>

#include <TableLogModel.h>

TableLogItemDelegate::TableLogItemDelegate(QObject* parent) :
   QStyledItemDelegate(parent), documents(4 * 1024 * 1024)
{
}

void TableLogItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const auto* model = static_cast<const TableLogModel*>(index.model());
   QStyleOptionViewItem opt = option;
   this->initStyleOption(&opt, index);
   QStyle* style = opt.widget ? opt.widget->style() : QApplication::style();

   if (index.column() != TableLogModel::MESSAGE)
   {
      if (index.column() == TableLogModel::SEVERITY)
      {
         switch (model->getSeverity(index.row()))
         {
         case LM::SV_END_USER: opt.backgroundBrush = QColor(41, 33, 53); break;
         case LM::SV_WARNING: opt.backgroundBrush = QColor(0, 47, 28); break;
         case LM::SV_ERROR: opt.backgroundBrush = QColor(200, 0, 0); break;
         case LM::SV_FATAL_ERROR: opt.backgroundBrush = QColor(50, 0, 0); break;
         default: break;
         }
      }
      style->drawControl(QStyle::CE_ItemViewItem, &opt, painter, opt.widget);
      return;
   }

   const auto doc = this->document(opt, index);
   opt.text.clear();
   style->drawControl(QStyle::CE_ItemViewItem, &opt, painter, opt.widget);

   QAbstractTextDocumentLayout::PaintContext ctx;
   ctx.palette = opt.palette;
   if (opt.state & QStyle::State_Selected)
      ctx.palette.setColor(QPalette::Text, opt.palette.color(QPalette::Active, QPalette::HighlightedText));

   // Highlight rendered text, without injecting markup into the log's HTML.
   if (!model->currentSearchTerm().isEmpty() && model->inSearchResult(index))
   {
      QTextCursor cursor(doc.data());
      while (!(cursor = doc->find(model->currentSearchTerm(), cursor)).isNull())
      {
         QAbstractTextDocumentLayout::Selection selection;
         selection.cursor = cursor;
         selection.format.setForeground(QColor("#FFFF00"));
         selection.format.setBackground(QColor("#21218B"));
         ctx.selections.append(selection);
      }
   }

   const QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &opt, opt.widget);
   painter->save();
   painter->translate(textRect.topLeft());
   painter->setClipRect(QRect(QPoint(0, 0), textRect.size()));
   ctx.clip = QRectF(0, 0, textRect.width(), textRect.height());
   doc->documentLayout()->draw(painter, ctx);
   painter->restore();
}

QSize TableLogItemDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (index.column() != TableLogModel::MESSAGE)
      return QStyledItemDelegate::sizeHint(option, index);

   QStyleOptionViewItem opt = option;
   this->initStyleOption(&opt, index);
   const auto doc = this->document(opt, index);
   return QSize(int(std::ceil(doc->idealWidth())) + 4, int(std::ceil(doc->size().height())) + 4);
}

void TableLogItemDelegate::resetSizesCache()
{
   this->documents.clear();
}

QSharedPointer<QTextDocument> TableLogItemDelegate::document(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (const auto* cached = this->documents.object(index))
      if (cached->text == option.text && cached->font == option.font)
         return cached->document;

   auto doc = QSharedPointer<QTextDocument>::create();
   doc->setDocumentMargin(2);
   doc->setDefaultFont(option.font);
   QTextOption textOption;
   textOption.setWrapMode(QTextOption::NoWrap);
   doc->setDefaultTextOption(textOption);
   doc->setHtml(option.text);
   doc->setTextWidth(-1); // Intrinsic, unwrapped layout is independent of column width.

   // Approximate layout cost, including text/formatting overhead. Oversized messages
   // are rendered but not retained. Shared ownership keeps cache eviction safe.
   const qint64 cost = 1024 + qint64(option.text.size()) * 2 + qint64(doc->characterCount()) * 64;
   if (cost <= this->documents.maxCost())
      this->documents.insert(index, new CachedDocument{option.text, option.font, doc}, int(cost));
   return doc;
}
