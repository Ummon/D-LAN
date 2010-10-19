#include "MainWindow.h"
#include "ui_MainWindow.h"
using namespace GUI;

#include <QTabBar>
#include <QMdiSubWindow>

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    this->ui->setupUi(this);

    this->lblStatusConnection = new QLabel(this->ui->statusBar);
    ui->statusBar->addWidget(this->lblStatusConnection);

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
    this->ui->tblPeers->setModel(&this->peerListModel);

    this->addDefaultWidgets();

    this->coreDisconnected(); // Initial state.

    connect(&this->coreConnection, SIGNAL(coreConnected()), this, SLOT(coreConnected()));
    connect(&this->coreConnection, SIGNAL(coreDisconnected()), this, SLOT(coreDisconnected()));
    connect(&this->coreConnection, SIGNAL(newState(Protos::GUI::State)), this, SLOT(newState(Protos::GUI::State)));
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

void MainWindow::newState(const Protos::GUI::State& state)
{
   this->peerListModel.setPeers(state.peer());
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
   this->widgetChat = new WidgetChat();
   QMdiSubWindow* subWin = this->ui->mdiArea->addSubWindow(this->widgetChat, Qt::CustomizeWindowHint);
   //subWin->layout()->set
   this->widgetChat->setWindowState(Qt::WindowMaximized);

   /* To see the close button on the tab.
   foreach (QTabBar* tab, ui->mdiArea->findChildren<QTabBar*>())
   {
      tab->setTabsClosable(true);
   }*/
}
