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
         // To draw the background (including the selection highlight).
         QStyledItemDelegate::paint(painter, newOption, index);

         if (index.data().isNull())
            return;

         int value = index.data().toInt();

         QStyleOptionProgressBar progressBarOption;
         progressBarOption.QStyleOption::operator=(newOption);
         progressBarOption.state |= QStyle::State_Horizontal;
         progressBarOption.minimum = 0;
         progressBarOption.maximum = 100;
         progressBarOption.textAlignment = Qt::AlignHCenter | Qt::AlignVCenter;
         progressBarOption.progress = value;
         progressBarOption.textVisible = false;

         // Do not use the entire surface to reduce the widget size.
         QRect rect(progressBarOption.rect);
         const int height = rect.height();
         rect.setTop(rect.top() + height / 4);
         rect.setBottom(rect.bottom() - height / 4);
         progressBarOption.rect = rect;

         QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter, option.widget);
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
         QStyleOptionViewItem optionV4 = option;
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
   return htmlText;
}

/////

void SearchMenu::onShowMenu(QMenu& menu)
{
   menu.addAction(QIcon(":/icons/ressources/folder.svg"), tr("Browse"), this, &SearchMenu::browse);
}

/////

SearchWidget::SearchWidget(
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   PeerListModel& peerListModel,
   const SharedEntryListModel& sharedEntryListModel,
   const Protos::Common::FindPattern& findPattern,
   bool local,
   QWidget* parent
) :
   QWidget(parent),
   ui(new Ui::SearchWidget),
   downloadMenu(sharedEntryListModel),
   coreConnection(coreConnection),
   searchModel(coreConnection, peerListModel, sharedEntryListModel)
{
   this->ui->setupUi(this);

   this->ui->lblSearchTerm->setText(SearchUtils::getFindPatternSummary(findPattern, local));

   const QString& terms = QString::fromStdString(findPattern.pattern());

   this->searchDelegate.setTerms(terms);

   connect(&this->searchModel, &SearchModel::progress, this, &SearchWidget::progress);

   this->ui->treeView->setModel(&this->searchModel);
   this->ui->treeView->setItemDelegate(&this->searchDelegate);
   this->ui->treeView->header()->setVisible(true);

   QList<quint32> columnSizes = SETTINGS.getRepeated<quint32>("search_column_sizes");
   if (columnSizes.size() != this->ui->treeView->header()->count())
      columnSizes = QList<quint32>() << 250 << 250 << 80 << 90 << 80;
   SETTINGS.set("search_column_sizes", columnSizes);
   SETTINGS.save();
   for (int i = 0; i < this->ui->treeView->header()->count(); i++)
      this->ui->treeView->header()->resizeSection(i, columnSizes[i]);

   connect(
      this->ui->treeView->selectionModel(),
      &QItemSelectionModel::selectionChanged,
      this,
      &SearchWidget::treeviewSelectionChanged
   );

   connect(
      this->ui->treeView->header(),
      &QHeaderView::sectionResized,
      this,
      &SearchWidget::treeviewSectionResized
   );

   this->ui->treeView->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->treeView->sortByColumn(SearchModel::RELEVANCE, Qt::AscendingOrder);

   this->searchModel.search(findPattern, local);

   this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->treeView, &QTreeView::customContextMenuRequested, this, &SearchWidget::displayContextMenuDownload);
   connect(this->ui->treeView, &QTreeView::doubleClicked, this, &SearchWidget::entryDoubleClicked);

   this->ui->butDownload->setEnabled(false);
   connect(this->ui->butDownload, &QPushButton::clicked, this, &SearchWidget::download);

   connect(&this->downloadMenu, &SearchMenu::download, this, &SearchWidget::download);
   connect(
      &this->downloadMenu,
      qOverload<>(&SearchMenu::downloadTo),
      this,
      qOverload<>(&SearchWidget::downloadTo)
   );
   connect(
      &this->downloadMenu,
      qOverload<const Common::Hash&>(&SearchMenu::downloadTo),
      this,
      qOverload<const Common::Hash&>(&SearchWidget::downloadTo)
   );
   connect(&this->downloadMenu, &SearchMenu::browse, this, &SearchWidget::browseCurrents);

   this->setWindowTitle(SearchUtils::getFindPatternWindowTitle(findPattern, local));
}

SearchWidget::~SearchWidget()
{
   disconnect(&this->searchModel, &SearchModel::progress, this->ui->prgSearch, &QProgressBar::setValue);
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
         menu.addAction(QIcon(":/icons/ressources/explore_folder.svg"), tr("Open location"), this, &SearchWidget::openLocation);
         menu.addAction(QIcon(":/icons/ressources/folder.svg"), tr("Browse"), this, &SearchWidget::browseCurrents);
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
         this->downloadTo(dirs.first());
      return;
   }

   for (const auto& index : this->ui->treeView->selectionModel()->selectedRows())
      this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index));
}

void SearchWidget::downloadTo()
{
   QStringList dirs = Utils::askForDirectoriesToDownloadTo(this->coreConnection);
   if (!dirs.isEmpty())
      this->downloadTo(dirs.first());
}
/**
  * Download all selected items to 'path'.
  */
void SearchWidget::downloadTo(const Common::Path& path)
{
   for (const auto& index : this->ui->treeView->selectionModel()->selectedRows())
      this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index), path);
}

/**
  * Download all selected items to the shared directory.
  */
void SearchWidget::downloadTo(const Common::Hash& sharedDirID)
{
   for (const auto& index : this->ui->treeView->selectionModel()->selectedRows())
      this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index), sharedDirID);
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

   Utils::openLocations(locations.values());
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
   SETTINGS.set("search_column_sizes", logicalIndex, static_cast<quint32>(newSize));
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
