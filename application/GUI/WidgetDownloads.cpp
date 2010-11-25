#include <WidgetDownloads.h>
#include <ui_WidgetDownloads.h>
using namespace GUI;

#include <QMenu>
#include <QMessageBox>

void DownloadsDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (!index.isValid())
      return;

   if (index.column() == 2)
   {
      Progress progress = index.data().value<Progress>();

      QStyleOptionProgressBarV2 progressBarOption;
      progressBarOption.rect = option.rect;
      progressBarOption.minimum = 0;
      progressBarOption.maximum = 100;
      progressBarOption.textAlignment = Qt::AlignHCenter;
      progressBarOption.progress = progress.progress;

      switch(progress.status)
      {
      case Protos::GUI::State_Download_Status_QUEUED:
         progressBarOption.text = "Queued";
         break;
      case Protos::GUI::State_Download_Status_INITIALIZING:
         progressBarOption.text = "Initializing..";
         break;
      case Protos::GUI::State_Download_Status_DOWNLOADING:
         progressBarOption.text = QString("%1%").arg(progress.progress);
         break;
      case Protos::GUI::State_Download_Status_COMPLETE:
         progressBarOption.text = "Complete";
         break;
      case Protos::GUI::State_Download_Status_PAUSED:
         progressBarOption.text = "Paused";
         break;
      default:
         progressBarOption.text = "Error";
         break;
      }

      progressBarOption.textVisible = true;

      QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter);
   }
   else
   {
      QStyleOptionViewItemV4 newOption(option);
      newOption.state = option.state & (~QStyle::State_HasFocus);
      QStyledItemDelegate::paint(painter, newOption, index);
   }
}

QSize DownloadsDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QSize size = QStyledItemDelegate::sizeHint(option, index);

   if (index.column() == 2)
      size.setWidth(100);
   return size;
}

/////

WidgetDownloads::WidgetDownloads(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetDownloads), coreConnection(coreConnection), downloadsModel(coreConnection, peerListModel)
{
   this->ui->setupUi(this);

   this->ui->tblDownloads->setModel(&this->downloadsModel);
   this->ui->tblDownloads->setItemDelegate(&this->downloadsDelegate);

   this->ui->tblDownloads->setDragEnabled(true);
   this->ui->tblDownloads->setDragDropMode(QAbstractItemView::InternalMove);
   this->ui->tblDownloads->setDropIndicatorShown(true);

   this->ui->tblDownloads->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblDownloads->horizontalHeader()->setVisible(false);
   this->ui->tblDownloads->horizontalHeader()->setResizeMode(0, QHeaderView::Stretch);
   this->ui->tblDownloads->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblDownloads->horizontalHeader()->setResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->tblDownloads->horizontalHeader()->setResizeMode(3, QHeaderView::ResizeToContents);

   //this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblDownloads->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblDownloads->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);

   this->ui->tblDownloads->verticalHeader()->setVisible(false);
   this->ui->tblDownloads->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblDownloads->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblDownloads->setShowGrid(false);
   this->ui->tblDownloads->setAlternatingRowColors(true);

   this->ui->tblDownloads->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblDownloads, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuDownloads(const QPoint&)));
}

WidgetDownloads::~WidgetDownloads()
{
   delete this->ui;
}

void WidgetDownloads::displayContextMenuDownloads(const QPoint& point)
{
   QMenu menu;
   menu.addAction("Remove selected entries", this, SLOT(removeSelectedEntries()));
   menu.addAction("Remove completed files", this, SLOT(removeCompletedFiles()));
   menu.exec(this->ui->tblDownloads->mapToGlobal(point));
}

void WidgetDownloads::removeSelectedEntries()
{
   QList<quint64> downloadIDs;

   QModelIndexList selectedRows = this->ui->tblDownloads->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      quint64 ID = this->downloadsModel.getDownloadID(i.next().row());
      if (ID != 0)
         downloadIDs << ID;
   }

   if (!downloadIDs.isEmpty())
   {
      QMessageBox msgBox(this);
      msgBox.setText("Are you sure to remove the selected downloads?");
      msgBox.setIcon(QMessageBox::Question);
      msgBox.setStandardButtons(QMessageBox::Ok | QMessageBox::Cancel);
      msgBox.setDefaultButton(QMessageBox::Ok);
      if (msgBox.exec() == QMessageBox::Ok)
      {
         this->coreConnection.cancelDownloads(downloadIDs);
      }
   }
}

void WidgetDownloads::removeCompletedFiles()
{
   this->coreConnection.cancelDownloads(this->downloadsModel.getCompletedDownloadIDs());
}
