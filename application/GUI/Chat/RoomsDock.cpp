#include <Chat/RoomsDock.h>
#include <ui_RoomsDock.h>
using namespace GUI;

RoomsDock::RoomsDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget *parent) :
   QDockWidget(parent),
   ui(new Ui::RoomsDock),
   coreConnection(coreConnection),
   roomsModel(this->coreConnection)
{
   this->ui->setupUi(this);

   this->ui->tblRooms->setModel(&this->roomsModel);
   this->ui->tblRooms->setItemDelegate(&this->roomsDelegate);
   this->ui->tblRooms->horizontalHeader()->setResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->tblRooms->horizontalHeader()->setResizeMode(1, QHeaderView::Stretch);
   this->ui->tblRooms->horizontalHeader()->setVisible(false);
   this->ui->tblRooms->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblRooms->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 4);
   this->ui->tblRooms->verticalHeader()->setVisible(false);
   this->ui->tblRooms->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblRooms->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblRooms->setShowGrid(false);
   this->ui->tblRooms->setAlternatingRowColors(false);
   //connect(this->ui->tblRooms->selectionModel(), SIGNAL(selectionChanged(QItemSelection, QItemSelection)), this, );
   //connect(this->ui->tblRooms, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(joinRoom()));
   connect(this->ui->butJoinRoom, SIGNAL(clicked()), this, SLOT(joinRoom()));

   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreDisconnected(false); // Initial state.
}

RoomsDock::~RoomsDock()
{
   delete ui;
}

void RoomsDock::joinRoom()
{
   QString roomName = this->ui->txtRoomName->text().trimmed();

   if (!roomName.isEmpty())
   {
      this->coreConnection->joinRoom(roomName);
      emit roomJoined(roomName);
   }
}

void RoomsDock::coreConnected()
{
   this->ui->butJoinRoom->setDisabled(false);
   this->ui->txtRoomName->setDisabled(false);
}

void RoomsDock::coreDisconnected(bool force)
{
   this->ui->butJoinRoom->setDisabled(true);
   this->ui->txtRoomName->setDisabled(true);
}
