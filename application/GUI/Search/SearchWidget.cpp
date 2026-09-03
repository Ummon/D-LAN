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

#include <algorithm>

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

/**
  * Folds 'text' the same way 'Common::StringUtils::splitInWords(..)' folds the search terms and fills 'positions'
  * with, for each character of the folded text, the position in 'text' of the character it comes from.
  * A character may fold to zero characters (a combining mark), to one, or to several ('½' gives "1/2",
  * a ligature gives its letters, ...), so the folded text and 'text' do NOT share their indices: 'positions' is
  * the only way to map a position in one back to the other.
  * 'positions' gets one extra element at the end, equal to the size of 'text', so the end of a match maps as well.
  */
static QString foldAndMapPositions(const QString& text, QList<int>& positions)
{
   QString foldedText;
   foldedText.reserve(text.size());
   positions.clear();
   positions.reserve(text.size() + 1);

   for (int i = 0; i < text.size();)
   {
      // A non-BMP character is stored as two 'QChar', they must be folded together.
      const int nbChars =
         text.at(i).isHighSurrogate() && i + 1 < text.size() && text.at(i + 1).isLowSurrogate() ? 2 : 1;

      if (nbChars == 1 && text.at(i).unicode() < 0x80) // ASCII is never expanded nor removed, no need to fold it.
      {
         foldedText += text.at(i).toLower();
         positions << i;
      }
      else
      {
         const QString foldedChars = Common::StringUtils::toLowerAndRemoveAccents(text.mid(i, nbChars));
         foldedText += foldedChars;
         for (int j = 0; j < foldedChars.size(); j++)
            positions << i;
      }

      i += nbChars;
   }

   positions << text.size();
   return foldedText;
}

/**
  * Put in bold each part of 'text' matching one of the current search terms.
  * The terms are searched in the folded text but the markup is inserted in 'text', at the positions given by
  * 'foldAndMapPositions(..)'.
  * 'text' is an entry name or path coming from a remote peer, every part of it is escaped so it cannot inject
  * any markup of its own into the document built by 'SearchDelegate::paint(..)' and 'sizeHint(..)'.
  */
QString SearchDelegate::toHtmlText(const QString& text) const
{
   QList<int> positions;
   const QString foldedText = foldAndMapPositions(text, positions);

   QList<QPair<int, int>> partsToHighlight; // Parts of 'text' to put in bold, as [begin, end[ positions.

   for (QStringListIterator i(this->currentTerms); i.hasNext();)
   {
      const QString& term = i.next();
      if (term.isEmpty()) // Would match at every position without ever advancing.
         continue;

      for (int pos = 0; (pos = foldedText.indexOf(term, pos)) != -1; pos += term.size())
         if (pos == 0 || !foldedText.at(pos - 1).isLetter()) // Only the terms beginning a word are highlighted.
            partsToHighlight << qMakePair(positions[pos], positions[pos + term.size()]);
   }

   if (partsToHighlight.isEmpty())
      return text.toHtmlEscaped();

   std::sort(partsToHighlight.begin(), partsToHighlight.end());

   QString htmlText;
   int current = 0; // Position in 'text' up to which 'htmlText' has been built.

   for (int i = 0; i < partsToHighlight.size(); i++)
   {
      const int begin = partsToHighlight[i].first;
      int end = partsToHighlight[i].second;

      // Two terms may match the same part of the text, the overlapping and contiguous parts are merged.
      while (i + 1 < partsToHighlight.size() && partsToHighlight[i + 1].first <= end)
         end = qMax(end, partsToHighlight[++i].second);

      htmlText += text.mid(current, begin - current).toHtmlEscaped();
      htmlText += MARKUP_FIRST_PART;
      htmlText += text.mid(begin, end - begin).toHtmlEscaped();
      htmlText += MARKUP_SECOND_PART;
      current = end;
   }

   htmlText += text.mid(current).toHtmlEscaped();

   return htmlText;
}

/////

void SearchMenu::onShowMenu(QMenu& menu)
{
   menu.addAction(QIcon(":/icons/resources/folder.svg"), tr("Browse"), this, &SearchMenu::browse);
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
         menu.addAction(QIcon(":/icons/resources/explore_folder.svg"), tr("Open location"), this, &SearchWidget::openLocation);
         menu.addAction(QIcon(":/icons/resources/folder.svg"), tr("Browse"), this, &SearchWidget::browseCurrents);
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
      this->downloadTo();
   else
      for (const auto& index : this->ui->treeView->selectionModel()->selectedRows())
         this->coreConnection->download(this->searchModel.getPeerID(index), this->searchModel.getEntry(index));
}

void SearchWidget::downloadTo()
{
   const QString dir = Utils::askForADirectoryToDownloadTo(this, this->coreConnection);
   if (!dir.isEmpty())
      this->downloadTo(dir);
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
