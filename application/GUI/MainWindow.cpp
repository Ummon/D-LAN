#include "MainWindow.h"
#include "ui_MainWindow.h"
using namespace GUI;

#include <QTabBar>
#include <QMdiSubWindow>
#include <QPainter>

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    peerListModel(coreConnection),
    chatModel(coreConnection, peerListModel)
{
    this->ui->setupUi(this);

    SETTINGS.setFilename("gui_settings.txt");
    SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
    SETTINGS.load();
    SETTINGS.save(); // To automatically create the file if it doesn't exist.

    this->lblStatusConnection = new QLabel(this->ui->statusBar);
    ui->statusBar->addWidget(this->lblStatusConnection);

    this->ui->tblPeers->setModel(&this->peerListModel);

    this->ui->tblPeers->setItemDelegate(new PeerTableDelegate());
    this->ui->tblPeers->horizontalHeader()->setResizeMode(QHeaderView::Stretch);
    this->ui->tblPeers->horizontalHeader()->setVisible(false);

    // TODO : is there an another way to reduce the row size?
    this->ui->tblPeers->verticalHeader()->setResizeMode(QHeaderView::Fixed);
    this->ui->tblPeers->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);

    this->ui->tblPeers->verticalHeader()->setVisible(false);
    this->ui->tblPeers->setSelectionBehavior(QAbstractItemView::SelectRows);
    this->ui->tblPeers->setSelectionMode(QAbstractItemView::SingleSelection);
    this->ui->tblPeers->setShowGrid(false);
    this->ui->tblPeers->setAlternatingRowColors(true);

    this->addDefaultWidgets();

    this->coreDisconnected(); // Initial state.

    connect(&this->coreConnection, SIGNAL(coreConnected()), this, SLOT(coreConnected()));
    connect(&this->coreConnection, SIGNAL(coreDisconnected()), this, SLOT(coreDisconnected()));
    this->coreConnection.connectToCore();
}

MainWindow::~MainWindow()
{
    delete this->ui;
}

void MainWindow::coreConnected()
{
   this->lblStatusConnection->setText("Connected");
}

void MainWindow::coreDisconnected()
{
   this->lblStatusConnection->setText("Disconnected");
}

/**
  * Add theses four default widgets to the MDI area :
  * - Settings
  * - Download
  * - Upload
  * - Chat
  */
void MainWindow::addDefaultWidgets()
{
   this->widgetSettings = new WidgetSettings(this->coreConnection, this);
   this->ui->mdiArea->addSubWindow(this->widgetSettings, Qt::CustomizeWindowHint);

   this->widgetChat = new WidgetChat(this->coreConnection, this->chatModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetChat, Qt::CustomizeWindowHint);
   this->widgetChat->setWindowState(Qt::WindowMaximized);

   /* To see the close button on the tab.
   foreach (QTabBar* tab, ui->mdiArea->findChildren<QTabBar*>())
   {
      tab->setTabsClosable(true);
   }*/
}

/**
  * Highlight ourself in the peers list.
  */
void PeerTableDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const PeerListModel* model = static_cast<const PeerListModel*>(index.model());

   QStyleOptionViewItemV4 newOption(option);

   if (model->isOurself(index.row()))
      painter->fillRect(option.rect, QColor(192, 255, 192));

   QItemDelegate::paint(painter, newOption, index);
}
