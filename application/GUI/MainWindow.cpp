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
#include <QMessageBox>
#include <QInputDialog>
#include <QColor>

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/RemoteCoreController/Builder.h>

#include <TabButtons.h>
#include <StatusBar.h>
#include <Log.h>

void PeerTableDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);

   // Show the selection only if the widget is active.
   if (!(newOption.state & QStyle::State_Active))
      newOption.state = newOption.state & (~QStyle::State_Selected);

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
   QApplication::instance()->installTranslator(&this->translator);

   this->ui->setupUi(this);

#ifdef Q_OS_DARWIN
   this->ui->butSearch->setMaximumWidth(24);
   this->ui->butSearchOwnFiles->setMaximumWidth(24);
#endif

   this->peerListModel.setSortType(static_cast<Protos::GUI::Settings::PeerSortType>(SETTINGS.get<quint32>("peer_sort_type")));

   this->mdiAreaTabBar = this->ui->mdiArea->findChild<QTabBar*>();
   this->mdiAreaTabBar->setMovable(true);
   this->mdiAreaTabBar->installEventFilter(this);
   connect(this->mdiAreaTabBar, SIGNAL(tabMoved(int, int)), this, SLOT(tabMoved(int, int)));

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
   this->ui->tblPeers->setSelectionMode(QAbstractItemView::ExtendedSelection);
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

   this->setApplicationStateAsDisconnected(); // Initial state.

   this->restoreWindowsSettings();

   this->restoreColorizedPeers();

   this->loadLanguage(this->widgetSettings->getCurrentLanguageFilename());

   connect(this->coreConnection.data(), SIGNAL(connectingError(RCC::ICoreConnection::ConnectionErrorCode)), this, SLOT(coreConnectionError(RCC::ICoreConnection::ConnectionErrorCode)));
   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreConnection->connectToCore(SETTINGS.get<QString>("core_address"), SETTINGS.get<quint32>("core_port"), SETTINGS.get<Common::Hash>("password"));
}

MainWindow::~MainWindow()
{
   this->saveWindowsSettings();

   this->coreConnection->disconnect(this); // Disconnect all signals.
   this->logModel.disconnect(this);

   this->removeWidgetSettings();

   delete this->ui;
}

void MainWindow::loadLanguage(const QString& filename)
{
   this->translator.load(filename, QCoreApplication::applicationDirPath() + "/" + Common::Constants::LANGUAGE_DIRECTORY);
}


void MainWindow::coreConnectionError(RCC::ICoreConnection::ConnectionErrorCode errorCode)
{
   QString error;
   switch (errorCode)
   {
   case RCC::ICoreConnection::ERROR_ALREADY_CONNECTED_TO_THIS_CORE:
      error = tr("Already connected to this address");
      break;
   case RCC::ICoreConnection::ERROR_CONNECTING_IN_PROGRESS:
      error = tr("There is already a connection process in progress");
      break;
   case RCC::ICoreConnection::ERROR_HOST_UNKOWN:
      error = tr("The host is unknow");
      break;
   case RCC::ICoreConnection::ERROR_HOST_TIMEOUT:
      error = tr("Host has timed out");
      break;
   case RCC::ICoreConnection::ERROR_NO_REMOTE_PASSWORD_DEFINED:
      error = tr("The host hasn't defined any password");
      break;
   case RCC::ICoreConnection::ERROR_WRONG_PASSWORD:
      error = tr("Wrong password");
      break;
   case RCC::ICoreConnection::ERROR_INVALID_ADDRESS:
      error = tr("Invalid address");
      break;
   case RCC::ICoreConnection::ERROR_UNKNOWN:
      error = tr("Error unknown");
   }

   QMessageBox msgBox(this);
   msgBox.setWindowTitle(tr("Unable to connect to the core"));
   msgBox.setText(QString("<p>%1</p><p>%2 <em>%3:%4</em></p>").arg(error).arg(tr("Remote core address:")).arg(this->coreConnection->getConnectionInfoConnecting().address).arg(this->coreConnection->getConnectionInfoConnecting().port));
   msgBox.setIcon(QMessageBox::Information);
   msgBox.setStandardButtons(QMessageBox::Ok);
   msgBox.exec();
}

void MainWindow::coreConnected()
{
   L_USER(tr("Connected to the core"));
   this->setApplicationStateAsConnected();
}

void MainWindow::coreDisconnected(bool forced)
{
   this->setApplicationStateAsDisconnected();

   if (!forced && !this->coreConnection->isConnecting())
   {
      QMessageBox msgBox(this);
      msgBox.setWindowTitle(tr("Connection lost"));
      msgBox.setText(QString("<p>%1</p><p>%2 <em>%3:%4</em></p>").arg(tr("The connection to the core has been lost")).arg(tr("Core address:")).arg(this->coreConnection->getConnectionInfo().address).arg(this->coreConnection->getConnectionInfo().port));
      msgBox.setIcon(QMessageBox::Information);
      msgBox.setStandardButtons(QMessageBox::Ok);
      msgBox.exec();
   }
}

void MainWindow::tabMoved(int, int)
{
   QList<quint32> values;

   for (int i = 0; i < this->mdiAreaTabBar->count(); i++)
   {
      QVariant data = this->mdiAreaTabBar->tabData(i);
      if (!data.isNull())
         values << data.toUInt();
   }

   SETTINGS.set("windowOrder", values);
   SETTINGS.save();
}

void MainWindow::displayContextMenuPeers(const QPoint& point)
{
   QMenu menu;
   menu.addAction(QIcon(":/icons/ressources/folder.png"), tr("Browse"), this, SLOT(browse()));

   QModelIndex i = this->ui->tblPeers->currentIndex();
   if (i.isValid() && this->peerListModel.getPeerID(i.row()) != this->coreConnection->getRemoteID())
   {
      QHostAddress addr = this->peerListModel.getPeerIP(i.row());
      QAction* takeControlAction = menu.addAction(QIcon(":/icons/ressources/lightning.png"), tr("Take control"), this, SLOT(takeControlOfACore()));
      QVariant data;
      data.setValue(addr);
      takeControlAction->setData(data);
   }

   menu.addSeparator();

   QAction* sortBySharingAmountAction = menu.addAction(tr("Sort by the amount of sharing"), this, SLOT(sortPeersBySharingAmount()));
   QAction* sortByNickAction = menu.addAction(tr("Sort alphabetically"), this, SLOT(sortPeersByNick()));

   sortBySharingAmountAction->setCheckable(true);
   sortBySharingAmountAction->setChecked(this->peerListModel.getSortType() == Protos::GUI::Settings::BY_SHARING_AMOUNT);

   sortByNickAction->setCheckable(true);
   sortByNickAction->setChecked(this->peerListModel.getSortType() == Protos::GUI::Settings::BY_NICK);

   menu.addSeparator();

   menu.addAction(QIcon(":/icons/ressources/marble_red.png"), tr("Colorize in red"), this, SLOT(colorizeSelectedPeer()))->setData(QColor(128, 0, 0));
   menu.addAction(QIcon(":/icons/ressources/marble_blue.png"), tr("Colorize in blue"), this, SLOT(colorizeSelectedPeer()))->setData(QColor(0, 0, 128));
   menu.addAction(QIcon(":/icons/ressources/marble_green.png"), tr("Colorize in green"), this, SLOT(colorizeSelectedPeer()))->setData(QColor(0, 128, 0));
   menu.addAction(tr("Uncolorize"), this, SLOT(uncolorizeSelectedPeer()));

   menu.exec(this->ui->tblPeers->mapToGlobal(point));
}

void MainWindow::browse()
{
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedIndexes())
   {
      if (i.isValid())
      {
         Common::Hash peerID = this->peerListModel.getPeerID(i.row());
         if (!peerID.isNull())
            this->addWidgetBrowse(peerID);
      }
   }

   this->ui->tblPeers->clearSelection();
}

void MainWindow::takeControlOfACore()
{
   QAction* action = dynamic_cast<QAction*>(this->sender());
   if (action)
   {
      QHostAddress address = action->data().value<QHostAddress>();
      QString password;

      if (!Common::Global::isLocal(address))
      {
         QInputDialog inputDialog(this);
         inputDialog.setWindowTitle(tr("Take control of %1").arg(Common::Global::formatIP(address, SETTINGS.get<quint32>("core_port"))));
         inputDialog.setLabelText(tr("Enter a password"));
         inputDialog.setTextEchoMode(QLineEdit::Password);
         inputDialog.resize(300, 100);

         if (inputDialog.exec() == QDialog::Rejected || inputDialog.textValue().isEmpty())
            return;

         password = inputDialog.textValue();
      }

      this->coreConnection->connectToCore(address.toString(), SETTINGS.get<quint32>("core_port"), password);
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

void MainWindow::sortPeersBySharingAmount()
{
   this->peerListModel.setSortType(Protos::GUI::Settings::BY_SHARING_AMOUNT);
   SETTINGS.set("peer_sort_type", static_cast<quint32>(Protos::GUI::Settings::BY_SHARING_AMOUNT));
   SETTINGS.save();
}

void MainWindow::sortPeersByNick()
{
   this->peerListModel.setSortType(Protos::GUI::Settings::BY_NICK);
   SETTINGS.set("peer_sort_type", static_cast<quint32>(Protos::GUI::Settings::BY_NICK));
   SETTINGS.save();
}

/**
  * Must be called only by a 'QAction' object whith a 'QColor' object as data.
  */
void MainWindow::colorizeSelectedPeer()
{
   const QColor color = static_cast<QAction*>(this->sender())->data().value<QColor>();

   QSet<Common::Hash> peerIDs;
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedIndexes())
   {
      this->peerListModel.colorize(i, color);
      peerIDs << this->peerListModel.getPeerID(i.row());
   }

   // Update the settings.
   Protos::GUI::Settings::HighlightedPeers highlightedPeers = SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peer_size() && !peerIDs.isEmpty(); i++)
   {
      const Common::Hash peerID(highlightedPeers.peer(i).id().hash());
      if (peerIDs.contains(peerID))
      {
         peerIDs.remove(peerID);
         highlightedPeers.mutable_peer(i)->set_color(color.rgb());
      }
   }

   foreach (Common::Hash peerID, peerIDs)
   {
      Protos::GUI::Settings::HighlightedPeers::Peer* peer = highlightedPeers.add_peer();
      peer->mutable_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
      peer->set_color(color.rgb());
   }

   SETTINGS.set("highlighted_peers", highlightedPeers);
   SETTINGS.save();

   this->ui->tblPeers->clearSelection();
}

void MainWindow::uncolorizeSelectedPeer()
{
   QSet<Common::Hash> peerIDs;
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedIndexes())
   {
      this->peerListModel.uncolorize(i);
      peerIDs << this->peerListModel.getPeerID(i.row());
   }

   // Update the settings.
   Protos::GUI::Settings::HighlightedPeers highlightedPeers = SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peer_size() && !peerIDs.isEmpty(); i++)
   {
      const Common::Hash peerID(highlightedPeers.peer(i).id().hash());
      if (peerIDs.contains(peerID))
      {
         peerIDs.remove(peerID);
         if (i != highlightedPeers.peer_size() - 1)
            highlightedPeers.mutable_peer()->SwapElements(i, highlightedPeers.peer_size() - 1);
         highlightedPeers.mutable_peer()->RemoveLast();
         i--;
      }
   }

   SETTINGS.set("highlighted_peers", highlightedPeers);
   SETTINGS.save();

   this->ui->tblPeers->clearSelection();
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
   if (obj == this->widgetChat && event->type() == QEvent::KeyPress)
   {
      this->keyPressEvent(static_cast<QKeyEvent*>(event));
      return event->isAccepted();
   }
   else if // Prohibits the user to close tab with the middle button.
   (
      obj == this->mdiAreaTabBar &&
      (event->type() == QEvent::MouseButtonPress || event->type() == QEvent::MouseButtonDblClick) &&
      static_cast<QMouseEvent*>(event)->button() == Qt::MiddleButton
   )
   {
      return true;
   }

   return QMainWindow::eventFilter(obj, event);
}

void MainWindow::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);
   else
      QWidget::changeEvent(event);
}

void MainWindow::setApplicationStateAsConnected()
{
   QList<quint32> windowsOrder = SETTINGS.getRepeated<quint32>("windowOrder");
   static const QList<quint32> windowsOrderDefault = QList<quint32>() <<
      Protos::GUI::Settings_Window_WIN_SETTINGS <<
      Protos::GUI::Settings_Window_WIN_CHAT <<
      Protos::GUI::Settings_Window_WIN_DOWNLOAD <<
      Protos::GUI::Settings_Window_WIN_UPLOAD;

   if (!QSet<quint32>::fromList(windowsOrder).contains(QSet<quint32>::fromList(windowsOrderDefault)))
      windowsOrder = windowsOrderDefault;

   for (QListIterator<quint32> i(windowsOrder); i.hasNext();)
   {
      switch (i.next())
      {
         case Protos::GUI::Settings_Window_WIN_SETTINGS: this->mdiAreaTabBar->moveTab(0, this->mdiAreaTabBar->count() - 1); break;
         case Protos::GUI::Settings_Window_WIN_CHAT: this->addWidgetChat(); break;
         case Protos::GUI::Settings_Window_WIN_DOWNLOAD: this->addWidgetDownloads(); break;
         case Protos::GUI::Settings_Window_WIN_UPLOAD: this->addWidgetUploads(); break;
      }
   }

   this->ui->txtSearch->setDisabled(false);
   this->ui->butSearch->setDisabled(false);
   this->ui->butSearchOwnFiles->setDisabled(false);
   this->ui->mdiArea->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
}

void MainWindow::setApplicationStateAsDisconnected()
{
   this->removeWidgetUploads();
   this->removeWidgetDownloads();
   this->removeWidgetChat();
   this->removeAllWidgets();
   this->ui->txtSearch->setDisabled(true);
   this->ui->butSearch->setDisabled(true);
   this->ui->butSearchOwnFiles->setDisabled(true);
   this->peerListModel.clear();
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

void MainWindow::restoreColorizedPeers()
{
   Protos::GUI::Settings::HighlightedPeers highlightedPeers = SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peer_size(); i++)
      this->peerListModel.colorize(highlightedPeers.peer(i).id().hash(), QColor(highlightedPeers.peer(i).color()));
}

/**
  * Remove and delete a sub window from the MDI area.
  */
void MainWindow::removeMdiSubWindow(QMdiSubWindow* mdiSubWindow)
{
   if (mdiSubWindow)
   {
      // Set a another sub window as active. If we don't do that the windows are all minimised (bug?).
      if (mdiSubWindow == this->ui->mdiArea->currentSubWindow())
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
   connect(this->widgetSettings, SIGNAL(languageChanged(QString)), this, SLOT(loadLanguage(QString)));
   this->ui->mdiArea->addSubWindow(this->widgetSettings, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_SETTINGS);
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
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_CHAT);
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
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_DOWNLOAD);
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
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_UPLOAD);
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

   QWidget* buttons = new QWidget();

   TabCloseButton* closeButton = new TabCloseButton(widgetBrowse, buttons);
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));

   TabRefreshButton* refreshButton = new TabRefreshButton(buttons);
   connect(refreshButton, SIGNAL(clicked()), widgetBrowse, SLOT(refresh()));

   QHBoxLayout* layButtons = new QHBoxLayout(buttons);
   layButtons->setContentsMargins(0, 0, 0, 0);
   layButtons->addWidget(refreshButton);
   layButtons->addWidget(closeButton);

   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, buttons);

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

   TabCloseButton* closeButton = new TabCloseButton(widgetSearch);
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, closeButton);

   return widgetSearch;
}

void MainWindow::removeAllWidgets()
{
   foreach (WidgetBrowse* widget, this->widgetsBrowse)
      this->removeWidget(widget);

   foreach (WidgetSearch* widget, this->widgetsSearch)
      this->removeWidget(widget);
}
