/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#include <Search/WidgetSearch.h>
#include <ui_WidgetSearch.h>
using namespace GUI;

#include <QTextDocument>
#include <QAbstractTextDocumentLayout>
#include <QRegExp>
#include <QMenu>
#include <QIcon>
#include <QDesktopServices>
#include <QUrl>
#include <QWindowsXPStyle>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Log.h>

const QString SearchDelegate::MARKUP_FIRST_PART("<b>");
const QString SearchDelegate::MARKUP_SECOND_PART("</b>");

void SearchDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   switch(index.column())
   {
   case 0:
      {
         this->initStyleOption(&newOption, index);

         QTextDocument doc;
         doc.setHtml(this->toHtmlText(newOption.text));

         // Painting item without text.
         newOption.text = QString();
         QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &newOption, painter, newOption.widget);

         QAbstractTextDocumentLayout::PaintContext ctx;

         // Highlighting text if item is selected and we are on Windows XP. TODO: find a better way.
         if (QString(QApplication::style()->metaObject()->className()) == "QWindowsXPStyle" && (newOption.state & QStyle::State_Selected))
            ctx.palette.setColor(QPalette::Text, newOption.palette.color(QPalette::Active, QPalette::HighlightedText));

         const QRect textRect = QApplication::style()->subElementRect(QStyle::SE_ItemViewItemText, &newOption);
         painter->save();
         painter->translate(textRect.topLeft());
         painter->setClipRect(textRect.translated(-textRect.topLeft()));
         doc.documentLayout()->draw(painter, ctx);
         painter->restore();
      }
      break;

   case 2: // Match rate.
      {
         QStyledItemDelegate::paint(painter, newOption, index);

         if (index.data().isNull())
            return;

         int value = index.data().toInt();

         QStyleOptionProgressBar progressBarOption;
         progressBarOption.QStyleOption::operator=(option);
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
      QStyledItemDelegate::paint(painter, newOption, index);
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
   this->currentTerms =  Common::Global::splitInWords(terms);
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

void SearchMenu::onShowMenu(QMenu& menu)
{
   menu.addAction(QIcon(":/icons/ressources/folder.png"), "Browse", this, SIGNAL(browse()));
}

/////

WidgetSearch::WidgetSearch(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const QString& terms, QWidget *parent) :
   QWidget(parent),
   ui(new Ui::WidgetSearch),
   menu(sharedDirsModel),
   coreConnection(coreConnection),
   searchModel(coreConnection, peerListModel, sharedDirsModel)
{
    this->ui->setupUi(this);
    this->searchDelegate.setTerms(terms);

    connect(&this->searchModel, SIGNAL(progress(int)), this, SLOT(progress(int)));

    this->ui->treeView->setModel(&this->searchModel);
    this->ui->treeView->setItemDelegate(&this->searchDelegate);
    this->ui->treeView->header()->setVisible(true);
    this->ui->treeView->header()->resizeSection(0, SETTINGS.get<quint32>("search_column_size_0"));
    this->ui->treeView->header()->resizeSection(1, SETTINGS.get<quint32>("search_column_size_1"));
    this->ui->treeView->header()->resizeSection(2, SETTINGS.get<quint32>("search_column_size_2"));
    this->ui->treeView->header()->resizeSection(3, SETTINGS.get<quint32>("search_column_size_3"));
    this->ui->treeView->header()->resizeSection(4, SETTINGS.get<quint32>("search_column_size_4"));
    connect(this->ui->treeView->header(), SIGNAL(sectionResized(int, int, int)), this, SLOT(treeviewSectionResized(int, int, int)));

    this->ui->treeView->setSelectionBehavior(QAbstractItemView::SelectRows);
    this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);

    this->searchModel.search(terms);

    this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(this->ui->treeView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuDownload(const QPoint&)));
    connect(this->ui->treeView, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(entryDoubleClicked(const QModelIndex&)));

    connect(this->ui->butDownload, SIGNAL(clicked()), this, SLOT(download()));    
    connect(&this->menu, SIGNAL(downloadTo(const Common::Hash&, const QString&)), this, SLOT(downloadTo(const Common::Hash&, const QString&)));
    connect(&this->menu, SIGNAL(browse()), this, SLOT(browseCurrents()));

    this->setWindowTitle(QString("\"%1\"").arg(terms));
}

WidgetSearch::~WidgetSearch()
{
   disconnect(&this->searchModel, SIGNAL(progress(int)), this->ui->prgSearch, SLOT(setValue(int)));
   delete this->ui;
}

void WidgetSearch::displayContextMenuDownload(const QPoint& point)
{   
   QPoint globalPosition = this->ui->treeView->viewport()->mapToGlobal(point);

   bool oneOfSeletedIsOur = false;
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      QModelIndex index = i.next();
      if (this->coreConnection->getID() == this->searchModel.getPeerID(index))
      {
         oneOfSeletedIsOur = true;
         break;
      }
   }

   // Special case: one of a selected entries is our.
   if (oneOfSeletedIsOur)
   {
      if (this->coreConnection->isLocal())
      {
         QMenu menu;
         menu.addAction(QIcon(":/icons/ressources/explore_folder.png"), "Open location", this, SLOT(openLocation()));
         menu.addAction(QIcon(":/icons/ressources/folder.png"), "Browse", this, SLOT(browseCurrents()));
         menu.exec(globalPosition);
      }
   }
   else
   {
      this->menu.show(globalPosition);
   }
}

void WidgetSearch::entryDoubleClicked(const QModelIndex& index)
{
   if (this->coreConnection->getID() == this->searchModel.getPeerID(index) && !this->searchModel.isDir(index))
      QDesktopServices::openUrl(QUrl("file:///" + this->searchModel.getPath(index), QUrl::TolerantMode));
}

void WidgetSearch::download()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      QModelIndex index = i.next();
      this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index));
   }
}

void WidgetSearch::downloadTo(const Common::Hash& sharedDirID, const QString& path)
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      QModelIndex index = i.next();
      this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index), sharedDirID, path);
   }
}

void WidgetSearch::openLocation()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();

   QSet<QString> locations;
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      QModelIndex index = i.next();
      locations.insert("file:///" + this->searchModel.getPath(index, false));
   }

   for (QSetIterator<QString> i(locations); i.hasNext();)
      QDesktopServices::openUrl(QUrl(i.next(), QUrl::TolerantMode));
}

void WidgetSearch::browseCurrents()
{
   QModelIndexList indexes = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(indexes); i.hasNext();)
   {
      QModelIndex index = i.next();
      emit browse(this->searchModel.getPeerID(index), this->searchModel.getEntry(index));
   }
}

void WidgetSearch::progress(int value)
{
   this->ui->prgSearch->setValue(value);
   const int nbFolders = this->searchModel.getNbFolders();
   const int nbFiles = this->searchModel.getNbFiles();
   this->ui->prgSearch->setFormat(QString("%1 folder%2 / %3 file%4").arg(nbFolders).arg(nbFolders > 1 ? "s" : "").arg(nbFiles).arg(nbFiles > 1 ? "s" : ""));
}

void WidgetSearch::treeviewSectionResized(int logicalIndex, int oldSize, int newSize)
{
   // TODO: use an array instead of multiple field.
   switch (logicalIndex)
   {
   case 0: SETTINGS.set("search_column_size_0", static_cast<quint32>(newSize)); break;
   case 1: SETTINGS.set("search_column_size_1", static_cast<quint32>(newSize)); break;
   case 2: SETTINGS.set("search_column_size_2", static_cast<quint32>(newSize)); break;
   case 3: SETTINGS.set("search_column_size_3", static_cast<quint32>(newSize)); break;
   case 4: SETTINGS.set("search_column_size_4", static_cast<quint32>(newSize)); break;
   }
}
