/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#include <WidgetSearch.h>
#include <ui_WidgetSearch.h>
using namespace GUI;

#include <QTextDocument>
#include <QAbstractTextDocumentLayout>
#include <QRegExp>
#include <QMenu>
#include <QIcon>

const QString SearchDelegate::MARKUP_FIRST_PART("<b>");
const QString SearchDelegate::MARKUP_SECOND_PART("</b>");

void SearchDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   switch(index.column())
   {
   case 0:
      {
         QStyleOptionViewItemV4 optionV4 = option;
         initStyleOption(&optionV4, index);

         const QWidget *widget = optionV4.widget;
         QStyle *style = widget ? widget->style() : QApplication::style();

         QTextDocument doc;
         doc.setHtml(this->toHtmlText(optionV4.text));

         /// Painting item without text
         optionV4.text = QString();
         style->drawControl(QStyle::CE_ItemViewItem, &optionV4, painter, widget);

         QAbstractTextDocumentLayout::PaintContext ctx;

         // Highlighting text if item is selected
         if (optionV4.state & QStyle::State_Selected)
             ctx.palette.setColor(QPalette::Text, optionV4.palette.color(QPalette::Active, QPalette::HighlightedText));

         QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &optionV4);
         painter->save();
         painter->translate(textRect.topLeft());
         painter->setClipRect(textRect.translated(-textRect.topLeft()));
         doc.documentLayout()->draw(painter, ctx);
         painter->restore();

      }
      break;

   case 2: // Match rate.
      {
         QStyledItemDelegate::paint(painter, option, index);

         if (index.data().isNull())
            return;

         int value = index.data().toInt();

         QStyleOptionProgressBar progressBarOption;
         progressBarOption.rect = option.rect;
         progressBarOption.minimum = 0;
         progressBarOption.maximum = 100;
         progressBarOption.textAlignment = Qt::AlignHCenter;
         progressBarOption.progress = value;
         progressBarOption.textVisible = false;

         QRect rect(progressBarOption.rect);
         const int height = rect.height();
         rect.setTop(rect.top() + height / 4);
         rect.setBottom(rect.bottom() - height / 4);
         progressBarOption.rect = rect;

         QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter);
      }
      break;

   default:
      QStyledItemDelegate::paint(painter, option, index);
   }
}

QSize SearchDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index ) const
{
   switch(index.column())
   {
   case 1:
      {
         QStyleOptionViewItemV4 optionV4 = option;
         initStyleOption(&optionV4, index);

         QTextDocument doc;
         doc.setHtml(this->toHtmlText(optionV4.text));
         //doc.setTextWidth(optionV4.rect.width());
         return QSize(doc.idealWidth() + 20, doc.size().height()); // + 20 is for the icon, TODO : find a better way to obtain this value.
      }
      break;

   default:
      return QStyledItemDelegate::sizeHint(option, index);
   }
}

void SearchDelegate::setTerms(const QString& terms)
{
   this->currentTerms = terms.split(' ', QString::SkipEmptyParts);
}

QString SearchDelegate::toHtmlText(const QString& text) const
{
   QString htmlText(text);
   for (QStringListIterator i(this->currentTerms); i.hasNext();)
   {
      const QString& term = i.next();
      int pos = 0;
      while(-1 != (pos = htmlText.indexOf(term, pos, Qt::CaseInsensitive)))
      {
         if (pos == 0 || !htmlText.at(pos-1).isLetter())
         {
            htmlText.insert(pos + term.size(), MARKUP_SECOND_PART);
            htmlText.insert(pos, MARKUP_FIRST_PART);
         }
         pos += term.size() + MARKUP_FIRST_PART.size() + MARKUP_SECOND_PART.size();
      }
   }
   return htmlText;
}

/////

WidgetSearch::WidgetSearch(CoreConnection& coreConnection, PeerListModel& peerListModel, const QString& terms, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetSearch), coreConnection(coreConnection), searchModel(coreConnection, peerListModel)
{
    this->ui->setupUi(this);
    this->searchDelegate.setTerms(terms);

    connect(&this->searchModel, SIGNAL(progress(int)), this, SLOT(progress(int)));

    this->ui->treeView->setModel(&this->searchModel);
    this->ui->treeView->setItemDelegate(&this->searchDelegate);
    this->ui->treeView->header()->setVisible(true);
    this->ui->treeView->header()->setResizeMode(0, QHeaderView::ResizeToContents);
    this->ui->treeView->header()->setResizeMode(1, QHeaderView::ResizeToContents);
    this->ui->treeView->header()->setResizeMode(2, QHeaderView::ResizeToContents);
    this->ui->treeView->header()->setResizeMode(3, QHeaderView::ResizeToContents);
    this->ui->treeView->header()->setResizeMode(4, QHeaderView::Stretch);

    this->searchModel.search(terms);

    this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(this->ui->treeView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuPeers(const QPoint&)));

    this->setWindowTitle(QString("\"%1\"").arg(terms));
}

WidgetSearch::~WidgetSearch()
{
   disconnect(&this->searchModel, SIGNAL(progress(int)), this->ui->prgSearch, SLOT(setValue(int)));
   delete this->ui;
}

void WidgetSearch::displayContextMenuPeers(const QPoint& point)
{
   QMenu menu;
   menu.addAction(QIcon(":/icons/ressources/download.png"), "Download", this, SLOT(download()));
   menu.exec(this->ui->treeView->mapToGlobal(point));
}

void WidgetSearch::download()
{
   QModelIndex i = this->ui->treeView->currentIndex();
   if (i.isValid())
   {
      this->coreConnection.download(this->searchModel.getPeerID(i), this->searchModel.getEntry(i));
   }
}

void WidgetSearch::progress(int value)
{
   this->ui->prgSearch->setValue(value);
   const int nbFolders = this->searchModel.getNbFolders();
   const int nbFiles = this->searchModel.getNbFiles();
   this->ui->prgSearch->setFormat(QString("%1 folder%2 / %3 file%4").arg(nbFolders).arg(nbFolders > 1 ? "s" : "").arg(nbFiles).arg(nbFiles > 1 ? "s" : ""));
}
