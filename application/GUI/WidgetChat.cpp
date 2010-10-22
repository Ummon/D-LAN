#include <WidgetChat.h>
#include <ui_WidgetChat.h>
using namespace GUI;

WidgetChat::WidgetChat(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent)
   :  QWidget(parent), ui(new Ui::WidgetChat), coreConnection(coreConnection), chatModel(coreConnection, peerListModel)
{
   this->ui->setupUi(this);

   this->ui->tblChat->setModel(&this->chatModel);

   this->ui->tblChat->setItemDelegate(new TblChatDelegate());
   this->ui->tblChat->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblChat->horizontalHeader()->setVisible(false);
   this->ui->tblChat->horizontalHeader()->setResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->tblChat->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblChat->horizontalHeader()->setResizeMode(2, QHeaderView::Stretch);

   //this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
   this->ui->tblChat->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblChat->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);

   this->ui->tblChat->verticalHeader()->setVisible(false);
   this->ui->tblChat->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblChat->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblChat->setShowGrid(false);
   this->ui->tblChat->setAlternatingRowColors(true);

   connect(&chatModel, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(newRows()));

   connect(this->ui->butSend, SIGNAL(clicked()), this, SLOT(sendMessage()));
   connect(this->ui->txtMessage, SIGNAL(returnPressed()), this, SLOT(sendMessage()));
}

WidgetChat::~WidgetChat()
{
   delete ui;
}

void WidgetChat::sendMessage()
{
   if (this->ui->txtMessage->text().isEmpty())
      return;

   static_cast<ChatModel*>(this->ui->tblChat->model())->newChatMessage(this->coreConnection.getOurID(), this->ui->txtMessage->text());

   this->coreConnection.sendChatMessage(this->ui->txtMessage->text());
   this->ui->txtMessage->clear();
}

void WidgetChat::newRows()
{
   this->ui->tblChat->scrollToBottom();
}
