#include <WidgetDownloads.h>
#include <ui_WidgetDownloads.h>
using namespace GUI;

void DownloadsDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (index.column() == 2)
   {
      Progress progress = index.data().value<Progress>();

       QStyleOptionProgressBar progressBarOption;
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
      QStyledItemDelegate::paint(painter, option, index);
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
   : QWidget(parent), ui(new Ui::WidgetDownloads), downloadsModel(coreConnection, peerListModel)
{
   this->ui->setupUi(this);

   this->ui->tblDownloads->setModel(&this->downloadsModel);
   this->ui->tblDownloads->setItemDelegate(&this->downloadsDelegate);
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
   this->ui->tblDownloads->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblDownloads->setShowGrid(false);
   this->ui->tblDownloads->setAlternatingRowColors(true);
}

WidgetDownloads::~WidgetDownloads()
{
   delete this->ui;
}
