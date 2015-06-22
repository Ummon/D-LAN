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
  
#include <Search/SearchWidget.h>
#include <ui_SearchWidget.h>
using namespace GUI;

#include <QTextDocument>
#include <QAbstractTextDocumentLayout>
#include <QRegExp>
#include <QMenu>
#include <QIcon>
#include <QUrl>

#include <Common/StringUtils.h>
#include <Common/Settings.h>

#include <Search/SearchUtils.h>
#include <Utils.h>
#include <Log.h>

const QString SearchDelegate::MARKUP_FIRST_PART("<b>");
const QString SearchDelegate::MARKUP_SECOND_PART("</b>");

void SearchDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (!index.isValid())
      return;

   QStyleOptionViewItem newOption = option;
   newOption.state = option.state & (~QStyle::State_HasFocus);
   this->initStyleOption(&newOption, index);

   switch (index.column())
   {
   case 0: // Item name.
      {
         QTextDocument doc;
         // We have to manually set the text color depending of the selection.
         doc.setDefaultStyleSheet(
            QString("span { color: %1 }").arg(newOption.state & QStyle::State_Selected ?
                                        #ifdef Q_OS_WIN32
                                                 newOption.palette.text().color().name() // FIXME: on Windows 'highlightedText' doesn't return the correct value.
                                        #else
                                                 newOption.palette.highlightedText().color().name()
                                        #endif
                                               : newOption.palette.text().color().name()
            )
         );
         doc.setHtml(this->toHtmlText(newOption.text));

         // Painting item without text.
         newOption.text = QString();
         QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &newOption, painter, newOption.widget);

         QAbstractTextDocumentLayout::PaintContext ctx;
         ctx.palette = newOption.palette;

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
         // To draw the background (including the selection highlight). We don't want to draw the text so we set its color to transparent.
         QPalette palette(newOption.palette);
         palette.setColor(QPalette::Active, QPalette::HighlightedText, QColor(0, 0, 0, 0));
         palette.setColor(QPalette::Active, QPalette::Text, QColor(0, 0, 0, 0));
         newOption.palette = palette;
         QStyledItemDelegate::paint(painter, newOption, index);

         if (index.data().isNull())
            return;

         int value = index.data().toInt();

         QStyleOptionProgressBarV2 progressBarOption;
         progressBarOption.QStyleOption::operator=(option);
         progressBarOption.minimum = 0;
         progressBarOption.maximum = 100;
         progressBarOption.textAlignment = Qt::AlignHCenter | Qt::AlignVCenter;
         progressBarOption.progress = value;
         progressBarOption.textVisible = false;

         QRect rect(progressBarOption.rect);
         const int height = rect.height();
         rect.setTop(rect.top() + height / 4);
         rect.setBottom(rect.bottom() - height / 4);
         progressBarOption.rect = rect;

         QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter, &this->model);
      }
      break;

   default:
      QStyledItemDelegate::paint(painter, newOption, index);
   }
}

QSize SearchDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index ) const
{
   switch (index.column())
   {
   case 1:
      {
         QStyleOptionViewItemV4 optionV4 = option;
         initStyleOption(&optionV4, index);

         QTextDocument doc;
         doc.setHtml(this->toHtmlText(optionV4.text));
         //doc.setTextWidth(optionV4.rect.width());
         return QSize(doc.idealWidth() + 20, doc.size().height()); // + 20 is for the icon, TODO: find a better way to obtain this value.
      }
      break;

   default:
      return QStyledItemDelegate::sizeHint(option, index);
   }
}

void SearchDelegate::setTerms(const QString& terms)
{
   this->currentTerms = Common::StringUtils::splitInWords(terms);
}

QString SearchDelegate::toHtmlText(const QString& text) const
{
   QString textWithoutAccent = Common::StringUtils::toLowerAndRemoveAccents(text);

   QString htmlText(text);

   for (QStringListIterator i(this->currentTerms); i.hasNext();)
   {
      const QString& term = i.next();
      int pos = 0;
      while(-1 != (pos = textWithoutAccent.indexOf(term, pos)))
      {
         if (pos == 0 || !htmlText.at(pos-1).isLetter())
         {
            htmlText.insert(pos + term.size(), MARKUP_SECOND_PART);
            htmlText.insert(pos, MARKUP_FIRST_PART);
            textWithoutAccent.insert(pos + term.size(), MARKUP_SECOND_PART);
            textWithoutAccent.insert(pos, MARKUP_FIRST_PART);
            pos += MARKUP_FIRST_PART.size() + MARKUP_SECOND_PART.size();
         }
         pos += term.size();
      }
   }
   return "<span>" + htmlText + "</span>";
}

/////

void SearchMenu::onShowMenu(QMenu& menu)
{
   menu.addAction(QIcon(":/icons/ressources/folder.png"), tr("Browse"), this, SIGNAL(browse()));
}

/////

SearchWidget::SearchWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const Protos::Common::FindPattern& findPattern, bool local, QWidget* parent) :
   QWidget(parent),
   ui(new Ui::SearchWidget),
   downloadMenu(sharedDirsModel),
   coreConnection(coreConnection),
   searchModel(coreConnection, peerListModel, sharedDirsModel)
{
   this->ui->setupUi(this);

   this->ui->lblSearchTerm->setText(SearchUtils::getFindPatternSummary(findPattern, local));

   const QString& terms = Common::ProtoHelper::getStr(findPattern, &Protos::Common::FindPattern::pattern);

   this->searchDelegate.setTerms(terms);

   connect(&this->searchModel, SIGNAL(progress(int)), this, SLOT(progress(int)));

   this->ui->treeView->setModel(&this->searchModel);
   this->ui->treeView->setItemDelegate(&this->searchDelegate);
   this->ui->treeView->header()->setVisible(true);

   QList<quint32> columnSizes = SETTINGS.getRepeated<quint32>("search_column_size");
   if (columnSizes.size() != this->ui->treeView->header()->count())
      columnSizes = QList<quint32>() << 275 << 200 << 60 << 80 << 80;
   SETTINGS.set("search_column_size", columnSizes);
   SETTINGS.save();
   for (int i = 0; i < this->ui->treeView->header()->count(); i++)
      this->ui->treeView->header()->resizeSection(i, columnSizes[i]);

   connect(this->ui->treeView->selectionModel(), SIGNAL(selectionChanged(QItemSelection, QItemSelection)), this, SLOT(treeviewSelectionChanged(QItemSelection, QItemSelection)));
   connect(this->ui->treeView->header(), SIGNAL(sectionResized(int, int, int)), this, SLOT(treeviewSectionResized(int, int, int)));

   this->ui->treeView->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->treeView->sortByColumn(SearchModel::toColumnNumber(SearchColumn::RELEVANCE), Qt::AscendingOrder);

   this->searchModel.search(findPattern, local);

   this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->treeView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuDownload(const QPoint&)));
   connect(this->ui->treeView, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(entryDoubleClicked(const QModelIndex&)));

   this->ui->butDownload->setEnabled(false);
   connect(this->ui->butDownload, SIGNAL(clicked()), this, SLOT(download()));

   connect(&this->downloadMenu, SIGNAL(download()), this, SLOT(download()));
   connect(&this->downloadMenu, SIGNAL(downloadTo()), this, SLOT(downloadTo()));
   connect(&this->downloadMenu, SIGNAL(downloadTo(const QString&, const Common::Hash&)), this, SLOT(downloadTo(const QString&, const Common::Hash&)));
   connect(&this->downloadMenu, SIGNAL(browse()), this, SLOT(browseCurrents()));

   this->setWindowTitle(SearchUtils::getFindPatternWindowTitle(findPattern));

   if (local)
      this->setWindowIcon(QIcon(":/icons/ressources/zoom_monitor.png"));
}

SearchWidget::~SearchWidget()
{
   disconnect(&this->searchModel, SIGNAL(progress(int)), this->ui->prgSearch, SLOT(setValue(int)));
   delete this->ui;
}

void SearchWidget::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);

   QWidget::changeEvent(event);
}

void SearchWidget::keyPressEvent(QKeyEvent* event)
{
   // Return key -> open all selected files.
   if (event->key() == Qt::Key_Return)
   {
      const QModelIndexList& selectedRows = this->ui->treeView->selectionModel()->selectedRows();
      for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
         this->openFile(i.next());
   }
   else
      QWidget::keyPressEvent(event);
}

void SearchWidget::displayContextMenuDownload(const QPoint& point)
{   
   QPoint globalPosition = this->ui->treeView->viewport()->mapToGlobal(point);

   // Special case: one of a selected entries is a remote peer.
   if (this->atLeastOneRemotePeer(this->ui->treeView->selectionModel()->selectedRows()))
   {
      this->downloadMenu.show(globalPosition);
   }
   else if (this->coreConnection->isLocal())
   {
      bool allSelectedEntriesAreTerminalFiles = true;
      const QModelIndexList& selectedRows = this->ui->treeView->selectionModel()->selectedRows();
      for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
         if (!SearchModel::isNonTerminalFile(i.next()))
         {
            allSelectedEntriesAreTerminalFiles = false;
            break;
         }

      if (!allSelectedEntriesAreTerminalFiles)
      {
         QMenu menu;
         menu.addAction(QIcon(":/icons/ressources/explore_folder.png"), tr("Open location"), this, SLOT(openLocation()));
         menu.addAction(QIcon(":/icons/ressources/folder.png"), tr("Browse"), this, SLOT(browseCurrents()));
         menu.exec(globalPosition);
      }
   }
}

void SearchWidget::entryDoubleClicked(const QModelIndex& index)
{
   this->openFile(index);
}

void SearchWidget::download()
{
   if (this->searchModel.nbSharedDirs() == 0)
   {
      QStringList dirs = Utils::askForDirectoriesToDownloadTo(this->coreConnection);
      if (!dirs.isEmpty())
         this->downloadTo(dirs.first(), Common::Hash());
      return;
   }

   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      QModelIndex index = i.next();
      this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index));
   }
}

void SearchWidget::downloadTo()
{
   QStringList dirs = Utils::askForDirectoriesToDownloadTo(this->coreConnection);
   if (!dirs.isEmpty())
      this->downloadTo(dirs.first());
}

void SearchWidget::downloadTo(const QString& path, const Common::Hash& sharedDirID)
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      QModelIndex index = i.next();
      this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index), sharedDirID, path);
   }
}

void SearchWidget::openLocation()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();

   QSet<QString> locations;
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      const QModelIndex& index = i.next();
      if (!SearchModel::isNonTerminalFile(index))
         locations.insert(this->searchModel.getPath(index, true));
   }

   Utils::openLocations(locations.toList());
}

void SearchWidget::browseCurrents()
{
   // We can only browse one item.
   QModelIndexList indexes = this->ui->treeView->selectionModel()->selectedRows();
   if (!indexes.isEmpty() && !SearchModel::isNonTerminalFile(indexes.first()))
      emit browse(this->searchModel.getPeerID(indexes.first()), this->searchModel.getEntry(indexes.first()));
}

void SearchWidget::progress(int value)
{
   this->ui->prgSearch->setValue(value);
   const int nbFolders = this->searchModel.getNbFolders();
   const int nbFiles = this->searchModel.getNbFiles();
   this->ui->prgSearch->setFormat(QString("%1 director%2 / %3 file%4").arg(nbFolders).arg(nbFolders > 1 ? "ies" : "y").arg(nbFiles).arg(nbFiles > 1 ? "s" : ""));
}

void SearchWidget::treeviewSelectionChanged(const QItemSelection& selected, const QItemSelection& deselected)
{
   this->ui->butDownload->setEnabled(this->atLeastOneRemotePeer(selected.indexes()));
}

void SearchWidget::treeviewSectionResized(int logicalIndex, int oldSize, int newSize)
{
   SETTINGS.set("search_column_size", logicalIndex, static_cast<quint32>(newSize));
   SETTINGS.save();
}

bool SearchWidget::atLeastOneRemotePeer(const QModelIndexList& indexes) const
{
   for (QListIterator<QModelIndex> i(indexes); i.hasNext();)
      if (this->searchModel.getPeerID(i.next()) != this->coreConnection->getRemoteID())
         return true;

   return false;
}

void SearchWidget::openFile(const QModelIndex& index) const
{
   if (!SearchModel::isNonTerminalFile(index) && this->coreConnection->getRemoteID() == this->searchModel.getPeerID(index) && !this->searchModel.isDir(index))
      Utils::openFile(this->searchModel.getPath(index));
}
