#include <Chat/RoomsDock.h>
#include <ui_RoomsDock.h>
using namespace GUI;

#include <QKeyEvent>
#include <QMenu>

RoomsDock::RoomsDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget *parent) :
   QDockWidget(parent),
   ui(new Ui::RoomsDock),
   coreConnection(coreConnection),
   roomsModel(this->coreConnection)
{
   this->ui->setupUi(this);

   this->ui->txtRoomName->installEventFilter(this);

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
   this->ui->tblRooms->setContextMenuPolicy(Qt::CustomContextMenu);

   connect(this->ui->tblRooms, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(displayContextMenuRooms(QPoint)));
   connect(this->ui->tblRooms, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(roomDoubleClicked(QModelIndex)));

   connect(this->ui->butJoinRoom, SIGNAL(clicked()), this, SLOT(joinRoom()));

   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreDisconnected(false); // Initial state.
}

RoomsDock::~RoomsDock()
{
   delete ui;
}

bool RoomsDock::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->ui->txtRoomName && event->type() == QEvent::KeyPress && static_cast<QKeyEvent*>(event)->key() == Qt::Key_Return)
   {
      this->joinRoom();
   }

   return QDockWidget::eventFilter(obj, event);
}

void RoomsDock::displayContextMenuRooms(const QPoint& point)
{
   QMenu menu;
   menu.addAction(QIcon(":/icons/ressources/join_chat_room.png"), tr("Join"), this, SLOT(joinSelectedRoom()));

   menu.exec(this->ui->tblRooms->mapToGlobal(point));
}

void RoomsDock::roomDoubleClicked(const QModelIndex& index)
{
   this->joinRoom(this->roomsModel.getRoomName(index));
}

void RoomsDock::joinSelectedRoom()
{
   QString roomName = this->roomsModel.getRoomName(this->ui->tblRooms->currentIndex());
   this->joinRoom(roomName);
}

void RoomsDock::joinRoom()
{
   this->joinRoom(this->ui->txtRoomName->text());
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

void RoomsDock::joinRoom(const QString& roomName)
{
   const QString cleanedName = roomName.trimmed().toLower();

   if (!cleanedName.isEmpty())
   {
      this->coreConnection->joinRoom(cleanedName);
      emit roomJoined(cleanedName);
   }
}
