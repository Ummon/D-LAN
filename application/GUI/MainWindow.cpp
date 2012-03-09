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
  
#include <MainWindow.h>
#include <ui_MainWindow.h>
using namespace GUI;

#include <QTabBar>
#include <QMdiSubWindow>
#include <QPainter>
#include <QMenu>
#include <QSettings>
#include <QHBoxLayout>
#include <QScrollBar>

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>
#include <Common/RemoteCoreController/Builder.h>

#include <TabButtons.h>
#include <StatusBar.h>
#include <Log.h>

/**
  * Highlight ourself in the peers list.
  */
void PeerTableDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const PeerListModel* model = static_cast<const PeerListModel*>(index.model());

   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   if (model->isOurself(index.row()))
      painter->fillRect(option.rect, QColor(192, 255, 192));

   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

void LogDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const LogModel* model = static_cast<const LogModel*>(index.model());

   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   switch (model->getSeverity(index.row()))
   {
   case LM::SV_WARNING:
      painter->fillRect(option.rect, QColor(235, 199, 199));
      break;
   case LM::SV_ERROR:
      painter->fillRect(option.rect, QColor(200, 0, 0));
      newOption.palette.setColor(QPalette::Text, QColor(255, 255, 255));
      break;
   case LM::SV_FATAL_ERROR:
      painter->fillRect(option.rect, QColor(50, 0, 0));
      newOption.palette.setColor(QPalette::Text, QColor(255, 255, 0));
      break;
   default:;
   }

   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

MainWindow::MainWindow(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QMainWindow(parent),
   ui(new Ui::MainWindow),
   widgetSettings(0),
   widgetChat(0),
   widgetDownloads(0),
   widgetUploads(0),
   coreConnection(coreConnection),
   peerListModel(coreConnection),
   autoScroll(true),
   logModel(coreConnection)
{
   this->ui->setupUi(this);

   StatusBar* statusBar = new StatusBar(this->coreConnection);
   ui->statusBar->addWidget(statusBar, 1);
   connect(statusBar, SIGNAL(showDockLog(bool)), this->ui->dockLog, SLOT(setVisible(bool)));

   this->ui->tblPeers->setModel(&this->peerListModel);

   this->ui->tblPeers->setItemDelegate(&this->peerTableDelegate);
   this->ui->tblPeers->horizontalHeader()->setResizeMode(0, QHeaderView::Stretch);
   this->ui->tblPeers->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblPeers->horizontalHeader()->setVisible(false);

   // TODO: is there an another way to reduce the row size?
   this->ui->tblPeers->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblPeers->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);
   this->ui->tblPeers->verticalHeader()->setVisible(false);
   this->ui->tblPeers->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblPeers->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblPeers->setShowGrid(false);
   this->ui->tblPeers->setAlternatingRowColors(true);

   this->ui->tblPeers->setContextMenuPolicy(Qt::CustomContextMenu);

   connect(this->ui->tblPeers, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuPeers(const QPoint&)));
   connect(this->ui->tblPeers, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(browse()));

   this->ui->tblLog->setModel(&this->logModel);

   this->ui->tblLog->setItemDelegate(&this->logDelegate);
   this->ui->tblLog->horizontalHeader()->setResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->tblLog->horizontalHeader()->setResizeMode(1, QHeaderView::Stretch);
   this->ui->tblLog->horizontalHeader()->setVisible(false);

   this->ui->tblLog->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblLog->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);
   this->ui->tblLog->verticalHeader()->setVisible(false);
   this->ui->tblLog->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblLog->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblLog->setShowGrid(false);
   this->ui->tblLog->setAlternatingRowColors(true);

   connect(&this->logModel, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(newLogMessage()));
   connect(this->ui->tblLog->verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(logScrollChanged(int)));
   connect(this->ui->dockLog, SIGNAL(visibilityChanged(bool)), statusBar, SLOT(dockLogVisibilityChanged(bool)));

   connect(this->ui->butSearch, SIGNAL(clicked()), this, SLOT(searchOtherPeers()));
   connect(this->ui->butSearchOwnFiles, SIGNAL(clicked()), this, SLOT(searchOwnFiles()));
   connect(this->ui->txtSearch, SIGNAL(returnPressed(Qt::KeyboardModifiers)), this, SLOT(txtSearchReturnPressed(Qt::KeyboardModifiers)));

   this->addWidgetSettings();

   this->coreDisconnected(); // Initial state.

   this->restoreWindowsSettings();

   this->ui->mdiArea->findChild<QTabBar*>()->setMovable(true);

   connect(this->coreConnection.data(), SIGNAL(coreConnected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(coreDisconnected()), this, SLOT(coreDisconnected()));

   this->coreConnection->connectToCore(SETTINGS.get<QString>("core_address"), SETTINGS.get<quint32>("core_port"), SETTINGS.get<Common::Hash>("password"));
}

MainWindow::~MainWindow()
{
   this->saveWindowsSettings();

   this->coreConnection->disconnect(this); // To avoid calling 'coreDisconnected' after deleted 'this->ui'.
   this->coreConnection->disconnectFromCore();
   this->logModel.disconnect(this);

   this->removeWidgetSettings();

   delete this->ui;
}

void MainWindow::coreConnected()
{
   this->addWidgetChat();
   this->addWidgetDownloads();
   this->addWidgetUploads();
   if (this->widgetSettings)
      this->widgetSettings->coreConnected();
   this->ui->txtSearch->setDisabled(false);
   this->ui->butSearch->setDisabled(false);
   this->ui->butSearchOwnFiles->setDisabled(false);
   this->ui->mdiArea->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
}

void MainWindow::coreDisconnected()
{
   this->removeWidgetUploads();
   this->removeWidgetDownloads();
   this->removeWidgetChat();
   this->removeAllWidgets();
   if (this->widgetSettings)
      this->widgetSettings->coreDisconnected();
   this->ui->txtSearch->setDisabled(true);
   this->ui->butSearch->setDisabled(true);
   this->ui->butSearchOwnFiles->setDisabled(true);
   this->peerListModel.clear();
}

void MainWindow::displayContextMenuPeers(const QPoint& point)
{
   QMenu menu;
   menu.addAction(QIcon(":/icons/ressources/folder.png"), "Browse", this, SLOT(browse()));
   menu.exec(this->ui->tblPeers->mapToGlobal(point));
}

void MainWindow::browse()
{
   QModelIndex i = this->ui->tblPeers->currentIndex();
   if (i.isValid())
   {
      Common::Hash peerID = this->peerListModel.getPeerID(i.row());
      if (!peerID.isNull())
         this->addWidgetBrowse(peerID);
   }
}

void MainWindow::searchOtherPeers()
{
   this->search(false);
}

void MainWindow::searchOwnFiles()
{
   this->search(true);
}

void MainWindow::txtSearchReturnPressed(Qt::KeyboardModifiers modifiers)
{
   if (modifiers.testFlag(Qt::ShiftModifier))
      this->searchOwnFiles();
   else
      this->searchOtherPeers();
}

/**
  * The widget can be a WidgetBrowse or a WidgetSearch.
  */
void MainWindow::removeWidget(QWidget* widget)
{
   WidgetBrowse* widgetBrowse;
   if (widgetBrowse = dynamic_cast<WidgetBrowse*>(widget))
      this->widgetsBrowse.removeOne(widgetBrowse);

   WidgetSearch* widgetSearch;
   if (widgetSearch = dynamic_cast<WidgetSearch*>(widget))
      this->widgetsSearch.removeOne(widgetSearch);

   this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(widget->parent()));
}

void MainWindow::logScrollChanged(int value)
{
   this->autoScroll = value == this->ui->tblLog->verticalScrollBar()->maximum();
}

void MainWindow::newLogMessage()
{
   if (this->autoScroll)
      this->ui->tblLog->scrollToBottom();
}

void MainWindow::keyPressEvent(QKeyEvent* event)
{
   // CTRL.
   if (event->modifiers().testFlag(Qt::ControlModifier))
   {
      switch (event->key())
      {
      // Search
      case 'f':
      case 'F':
         this->ui->txtSearch->setFocus();
         this->ui->txtSearch->selectAll();
         return;

      // Close the current window.
      case 'w':
      case 'W':
         if (this->ui->mdiArea->currentSubWindow())
         {
            QWidget* widget = this->ui->mdiArea->currentSubWindow()->widget();

            if (dynamic_cast<WidgetBrowse*>(widget) || dynamic_cast<WidgetSearch*>(widget))
               this->removeWidget(widget);
         }
         return;

      default:
         // Focus the nth window.
         if (event->key() >= '1' && event->key() <= '9')
         {
            const int num = event->key() - '1';
            if (num < this->ui->mdiArea->subWindowList().size())
               this->ui->mdiArea->setActiveSubWindow(this->ui->mdiArea->subWindowList()[num]);
            return;
         }
      }
   }

   QMainWindow::keyPressEvent(event);
}

void MainWindow::closeEvent(QCloseEvent* event)
{
   delete this;
}

void MainWindow::search(bool ownFiles)
{
   this->ui->txtSearch->setText(this->ui->txtSearch->text().trimmed());

   if (!this->ui->txtSearch->text().isEmpty())
   {
      this->addWidgetSearch(this->ui->txtSearch->text(), ownFiles);
   }
}

bool MainWindow::eventFilter(QObject* obj, QEvent* event)
{
   if (event->type() == QEvent::KeyPress)
   {
      this->keyPressEvent(static_cast<QKeyEvent*>(event));
      return event->isAccepted();
   }
   return QMainWindow::eventFilter(obj, event);
}

void MainWindow::saveWindowsSettings()
{
   L_DEBU(QString("Save state : %1").arg(QString::fromAscii(this->saveState().toHex().data())));

   SETTINGS.set("windows_state", this->saveState());

   // Qt doc says maximized property only works on Windows.
#ifdef Q_OS_WIN32
   SETTINGS.set("main_window_maximized", this->isMaximized());
   if (!this->isMaximized())
#endif
   {
      SETTINGS.set("main_window_width", static_cast<quint32>(this->size().width()));
      SETTINGS.set("main_window_height", static_cast<quint32>(this->size().height()));
   }

   SETTINGS.save();
}

void MainWindow::restoreWindowsSettings()
{
   this->resize(QSize(SETTINGS.get<quint32>("main_window_width"), SETTINGS.get<quint32>("main_window_height")));

#ifdef Q_OS_WIN32
   if (SETTINGS.get<bool>("main_window_maximized"))
      this->showMaximized();
#endif

   QByteArray state = SETTINGS.get<QByteArray>("windows_state");
   if (state.isEmpty())
      state = QByteArray::fromHex("000000ff00000000fd0000000200000000000000bf000000e1fc0200000002fb000000140064006f0063006b00530065006100720063006801000000000000001c0000001c0000001cfb000000120064006f0063006b005000650065007200730100000020000000c10000004b00ffffff00000003000003840000005dfc0100000001fb0000000e0064006f0063006b004c006f00670000000000000003840000006100ffffff000002c1000000e100000004000000040000000800000008fc00000000");
   this->restoreState(state);
}

/**
  * Remove and delete a sub window from the MDI area.
  */
void MainWindow::removeMdiSubWindow(QMdiSubWindow* mdiSubWindow)
{
   if (mdiSubWindow)
   {
      // Set a another sub window as active. If we don't do that the windows are all minimised (bug?).
      if (mdiSubWindow == this->ui->mdiArea->currentSubWindow());
      {
         QList<QMdiSubWindow*> subWindows = this->ui->mdiArea->subWindowList();
         if (subWindows.size() > 1)
         {
            int i = subWindows.indexOf(mdiSubWindow);
            if (i <= 0)
               this->ui->mdiArea->setActiveSubWindow(subWindows[i+1]);
            else
               this->ui->mdiArea->setActiveSubWindow(subWindows[i-1]);
         }
      }

      this->ui->mdiArea->removeSubWindow(mdiSubWindow);
      delete mdiSubWindow;
   }
}

void MainWindow::addWidgetSettings()
{
   this->widgetSettings = new WidgetSettings(this->coreConnection, this->sharedDirsModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetSettings, Qt::CustomizeWindowHint);
   this->widgetSettings->setWindowState(Qt::WindowMaximized);
}

void MainWindow::removeWidgetSettings()
{
   if (this->widgetSettings)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetSettings->parent()));
      this->widgetSettings = 0;
   }
}

void MainWindow::addWidgetChat()
{
   this->widgetChat = new WidgetChat(this->coreConnection, this->peerListModel, this);
   this->widgetChat->installEventFilterOnInput(this);
   this->ui->mdiArea->addSubWindow(this->widgetChat, Qt::CustomizeWindowHint);
   this->widgetChat->setWindowState(Qt::WindowMaximized);
}

void MainWindow::removeWidgetChat()
{
   if (this->widgetChat)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
      this->widgetChat = 0;
   }
}

void MainWindow::addWidgetDownloads()
{
   this->widgetDownloads = new WidgetDownloads(this->coreConnection, this->peerListModel, this->sharedDirsModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetDownloads, Qt::CustomizeWindowHint);
   this->widgetDownloads->setWindowState(Qt::WindowMaximized);
}

void MainWindow::removeWidgetDownloads()
{
   if (this->widgetDownloads)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetDownloads->parent()));
      this->widgetDownloads = 0;
   }
}

void MainWindow::addWidgetUploads()
{
   this->widgetUploads = new WidgetUploads(this->coreConnection, this->peerListModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetUploads, Qt::CustomizeWindowHint);
   this->widgetUploads->setWindowState(Qt::WindowMaximized);
}

void MainWindow::removeWidgetUploads()
{
   if (this->widgetUploads)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetUploads->parent()));
      this->widgetUploads = 0;
   }
}

WidgetBrowse* MainWindow::addWidgetBrowse(const Common::Hash& peerID)
{
   // If there is already a browse for the given peer we show it.
   for (QListIterator<WidgetBrowse*> i(this->widgetsBrowse); i.hasNext();)
   {
      WidgetBrowse* widget = i.next();
      if (widget->getPeerID() == peerID)
      {
         widget->refresh();
         this->ui->mdiArea->setActiveSubWindow(static_cast<QMdiSubWindow*>(widget->parent()));
         return widget;
      }
   }

   WidgetBrowse* widgetBrowse = new WidgetBrowse(this->coreConnection, this->peerListModel, this->sharedDirsModel, peerID, this);
   this->ui->mdiArea->addSubWindow(widgetBrowse, Qt::CustomizeWindowHint);
   widgetBrowse->setWindowState(Qt::WindowMaximized);
   this->widgetsBrowse << widgetBrowse;

   QTabBar* tab = this->ui->mdiArea->findChild<QTabBar*>();

   QWidget* buttons = new QWidget();

   TabCloseButton* closeButton = new TabCloseButton(widgetBrowse, buttons);
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));

   TabRefreshButton* refreshButton = new TabRefreshButton(buttons);
   connect(refreshButton, SIGNAL(clicked()), widgetBrowse, SLOT(refresh()));

   QHBoxLayout* layButtons = new QHBoxLayout(buttons);
   layButtons->setContentsMargins(0, 0, 0, 0);
   layButtons->addWidget(refreshButton);
   layButtons->addWidget(closeButton);

   tab->setTabButton(tab->count() - 1, QTabBar::RightSide, buttons);

   return widgetBrowse;
}

WidgetBrowse* MainWindow::addWidgetBrowse(const Common::Hash& peerID, const Protos::Common::Entry& remoteEntry)
{
   WidgetBrowse* widgetBrowse = this->addWidgetBrowse(peerID);
   widgetBrowse->browseTo(remoteEntry);
   return widgetBrowse;
}

WidgetSearch* MainWindow::addWidgetSearch(const QString& term, bool searchInOwnFiles)
{
   WidgetSearch* widgetSearch = new WidgetSearch(this->coreConnection, this->peerListModel, this->sharedDirsModel, term, searchInOwnFiles, this);
   this->ui->mdiArea->addSubWindow(widgetSearch, Qt::CustomizeWindowHint);
   widgetSearch->setWindowState(Qt::WindowMaximized);
   this->widgetsSearch << widgetSearch;
   connect(widgetSearch, SIGNAL(browse(const Common::Hash&, const Protos::Common::Entry&)), this, SLOT(addWidgetBrowse(const Common::Hash&, const Protos::Common::Entry&)));

   QTabBar* tab = ui->mdiArea->findChild<QTabBar*>();
   TabCloseButton* closeButton = new TabCloseButton(widgetSearch);
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));
   tab->setTabButton(tab->count() - 1, QTabBar::RightSide, closeButton);

   return widgetSearch;
}

void MainWindow::removeAllWidgets()
{
   foreach (WidgetBrowse* widget, this->widgetsBrowse)
      this->removeWidget(widget);

   foreach (WidgetSearch* widget, this->widgetsSearch)
      this->removeWidget(widget);
}
