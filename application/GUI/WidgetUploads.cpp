#include <WidgetUploads.h>
#include <ui_WidgetUploads.h>
using namespace GUI;

void UploadsDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (index.column() == 2)
   {
       QStyleOptionProgressBar progressBarOption;
       progressBarOption.rect = option.rect;
       progressBarOption.minimum = 0;
       progressBarOption.maximum = 100;
       progressBarOption.textAlignment = Qt::AlignHCenter;
       progressBarOption.progress = index.data().toInt();
       progressBarOption.textVisible = false;

       QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter);
   }
   else
   {
      QStyledItemDelegate::paint(painter, option, index);
   }
}

QSize UploadsDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QSize size = QStyledItemDelegate::sizeHint(option, index);

   if (index.column() == 2)
      size.setWidth(100);
   return size;
}

/////

WidgetUploads::WidgetUploads(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetUploads), uploadsModel(coreConnection, peerListModel)
{
   this->ui->setupUi(this);

   this->ui->tblUploads->setModel(&this->uploadsModel);
   this->ui->tblUploads->setItemDelegate(&this->uploadsDelegate);
   this->ui->tblUploads->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblUploads->horizontalHeader()->setVisible(false);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(0, QHeaderView::Stretch);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->tblUploads->horizontalHeader()->setResizeMode(3, QHeaderView::ResizeToContents);

   //this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblUploads->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblUploads->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);

   this->ui->tblUploads->verticalHeader()->setVisible(false);
   this->ui->tblUploads->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblUploads->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblUploads->setShowGrid(false);
   this->ui->tblUploads->setAlternatingRowColors(true);
}

WidgetUploads::~WidgetUploads()
{
   delete this->ui;
}
