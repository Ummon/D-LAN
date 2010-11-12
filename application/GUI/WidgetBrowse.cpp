#include <WidgetBrowse.h>
#include <ui_WidgetBrowse.h>
using namespace GUI;

#include <QMenu>
#include <QIcon>

void BrowseDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

WidgetBrowse::WidgetBrowse(CoreConnection& coreConnection, PeerListModel& peerListModel, const Common::Hash& peerID, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetBrowse), coreConnection(coreConnection), peerListModel(peerListModel), peerID(peerID), browseModel(coreConnection, peerID)
{
   this->ui->setupUi(this);

   this->ui->treeView->setModel(&this->browseModel);
   this->ui->treeView->setItemDelegate(&this->browseDelegate);
   this->ui->treeView->header()->setVisible(false);
   this->ui->treeView->header()->setResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->treeView->header()->setResizeMode(1, QHeaderView::Stretch);

   this->ui->treeView->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);

   this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->treeView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuPeers(const QPoint&)));

   this->setWindowTitle(QString("[%1]").arg(this->peerListModel.getNick(this->peerID)));
}

WidgetBrowse::~WidgetBrowse()
{
    delete this->ui;
}

Common::Hash WidgetBrowse::getPeerID() const
{
   return this->peerID;
}

void WidgetBrowse::displayContextMenuPeers(const QPoint& point)
{
   QMenu menu;
   menu.addAction(QIcon(":/icons/ressources/download.png"), "Download selected entries", this, SLOT(download()));
   menu.exec(this->ui->treeView->mapToGlobal(point));
}

void WidgetBrowse::download()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      this->coreConnection.download(this->peerID, this->browseModel.getEntry(i.next()));
   }
}
