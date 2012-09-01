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

#include <cmath>

#include <QTabBar>
#include <QClipboard>
#include <QStringBuilder>
#include <QMdiSubWindow>
#include <QPainter>
#include <QMenu>
#include <QSettings>
#include <QHBoxLayout>
#include <QScrollBar>
#include <QMessageBox>
#include <QColor>
#include <QPen>

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/RemoteCoreController/Builder.h>

#include <WidgetDocument.h>
#include <TabButtons.h>
#include <StatusBar.h>
#include <Log.h>

MainWindow::MainWindow(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QMainWindow(parent),
   coreConnection(coreConnection),
   ui(new Ui::MainWindow),
   searchDock(new SearchDock(this->coreConnection, this)),
   peersDock(new PeersDock(this->coreConnection, this)),
   roomsDock(new RoomsDock(this->coreConnection, this)),
   widgetSettings(0),
   widgetChat(0),
   widgetDownloads(0),
   widgetUploads(0),
   downloadsBusyIndicator(0),
   customStyleLoaded(false),
   autoScroll(true),
   logModel(coreConnection)
{
   this->ui->setupUi(this);

   this->initialWindowFlags = this->windowFlags();

   this->ui->mdiArea->setOption(QMdiArea::DontMaximizeSubWindowOnActivation, true);
   connect(this->ui->mdiArea, SIGNAL(subWindowActivated(QMdiSubWindow*)), this, SLOT(subWindowActivated(QMdiSubWindow*)));

   this->mdiAreaTabBar = this->ui->mdiArea->findChild<QTabBar*>();
   this->mdiAreaTabBar->setMovable(true);
   this->mdiAreaTabBar->installEventFilter(this);
   connect(this->mdiAreaTabBar, SIGNAL(tabMoved(int, int)), this, SLOT(tabMoved(int, int)));

   StatusBar* statusBar = new StatusBar(this->coreConnection);
   ui->statusBar->addWidget(statusBar, 1);
   connect(statusBar, SIGNAL(showDockLog(bool)), this->ui->dockLog, SLOT(setVisible(bool)));

   ///// Dockable widgets
   this->addDockWidget(Qt::LeftDockWidgetArea, this->searchDock);
   connect(this->searchDock, SIGNAL(search(QString, bool)), this, SLOT(search(QString, bool)));
   this->addDockWidget(Qt::LeftDockWidgetArea, this->peersDock);
   connect(this->peersDock, SIGNAL(browsePeer(Common::Hash)), this, SLOT(browsePeer(Common::Hash)));
   this->addDockWidget(Qt::LeftDockWidgetArea, this->roomsDock);
   connect(this->roomsDock, SIGNAL(roomJoined(QString)), this, SLOT(roomJoined(QString)));
   /////

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
   // If we didn't set auto-scroll to 'false', when the selection is on the first item and it being deleted, the selection automatically change to the next item and the view scroll to it.
   this->ui->tblLog->setAutoScroll(false);
   this->ui->tblLog->setAlternatingRowColors(true);

   connect(&this->logModel, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(newLogMessage()));
   connect(this->ui->tblLog->verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(logScrollChanged(int)));
   connect(this->ui->dockLog, SIGNAL(visibilityChanged(bool)), statusBar, SLOT(dockLogVisibilityChanged(bool)));

   this->addWidgetSettings();

   this->setApplicationStateAsDisconnected(); // Initial state.

   this->ui->grip->setVisible(false);
   this->ui->grip->installEventFilter(this);
   connect(this->ui->butClose, SIGNAL(clicked()), this, SLOT(close()));
   connect(this->ui->butMinimize, SIGNAL(clicked()), this, SLOT(showMinimized()));
   connect(this->ui->butMaximize, SIGNAL(clicked()), this, SLOT(maximize()));
   if (!SETTINGS.get<QString>("style").isEmpty())
      this->loadCustomStyle(QCoreApplication::applicationDirPath() % "/" % Common::Constants::STYLE_DIRECTORY % "/" % SETTINGS.get<QString>("style") % "/" % Common::Constants::STYLE_FILE_NAME);

   this->restoreWindowsSettings();

   connect(this->coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(this->coreConnection.data(), SIGNAL(connectingError(RCC::ICoreConnection::ConnectionErrorCode)), this, SLOT(coreConnectionError(RCC::ICoreConnection::ConnectionErrorCode)));
   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreConnection->connectToCore(SETTINGS.get<QString>("core_address"), SETTINGS.get<quint32>("core_port"), SETTINGS.get<Common::Hash>("password"));

#ifdef DEBUG
   QPushButton* logEntireQWidgetTreeButton = new QPushButton();
   logEntireQWidgetTreeButton->setText("log widget tree");
   connect(logEntireQWidgetTreeButton, SIGNAL(clicked()), this, SLOT(logEntireQWidgetTree()));
   this->ui->statusBar->addWidget(logEntireQWidgetTreeButton);
#endif
}

MainWindow::~MainWindow()
{
   this->saveWindowsSettings();

   this->coreConnection->disconnect(this); // Disconnect all signals.
   this->logModel.disconnect(this);

   this->removeWidgetSettings();

   delete this->ui;
}

void MainWindow::newState(const Protos::GUI::State& state)
{
   // Synchronize the joined chat rooms with the opened windows.
   // There is two reasons for doing that:
   //  1) The joined room may be persisted by the core.
   //  2) Another GUI connected to the same core may open or close a chat room.
   QSet<QString> joinedRooms;
   for (int i = 0; i < state.rooms_size(); i++)
      if (state.rooms(i).joined())
         joinedRooms.insert(Common::ProtoHelper::getStr(state.rooms(i), &Protos::GUI::State::Room::name));

   foreach (WidgetChat* widgetChat, this->widgetsChatRoom)
      if (!joinedRooms.remove(widgetChat->getRoomName()))
         this->removeWidget(widgetChat);

   foreach (QString roomName, joinedRooms)
      this->addWidgetChatRoom(roomName, false);

   if (this->downloadsBusyIndicator)
   {
      if (state.stats().cache_status() == Protos::GUI::State::Stats::LOADING_CACHE_IN_PROGRESS)
         this->downloadsBusyIndicator->show();
      else
         this->downloadsBusyIndicator->hide();
   }
}

void MainWindow::onGlobalProgressChanged(quint64 completed, quint64 total)
{
   if (total == 0 || completed == total)
   {
      this->taskbar.setStatus(TaskbarButtonStatus::BUTTON_STATUS_NOPROGRESS);
   }
   else
   {
      this->taskbar.setStatus(TaskbarButtonStatus::BUTTON_STATUS_NORMAL);
      this->taskbar.setProgress(completed, total);
   }
}

void MainWindow::coreConnectionError(RCC::ICoreConnection::ConnectionErrorCode errorCode)
{
   QString error;
   switch (errorCode)
   {
   case RCC::ICoreConnection::RCC_ERROR_ALREADY_CONNECTED_TO_THIS_CORE:
      error = tr("Already connected to this address");
      break;
   case RCC::ICoreConnection::RCC_ERROR_CONNECTING_IN_PROGRESS:
      error = tr("There is already a connection process in progress");
      break;
   case RCC::ICoreConnection::RCC_ERROR_HOST_UNKOWN:
      error = tr("The host is unknow");
      break;
   case RCC::ICoreConnection::RCC_ERROR_HOST_TIMEOUT:
      error = tr("Host has timed out");
      break;
   case RCC::ICoreConnection::RCC_ERROR_NO_REMOTE_PASSWORD_DEFINED:
      error = tr("The host hasn't defined any password");
      break;
   case RCC::ICoreConnection::RCC_ERROR_WRONG_PASSWORD:
      error = tr("Wrong password");
      break;
   case RCC::ICoreConnection::RCC_ERROR_INVALID_ADDRESS:
      error = tr("Invalid address");
      break;
   case RCC::ICoreConnection::RCC_ERROR_UNKNOWN:
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

   if (this->downloadsBusyIndicator)
      this->downloadsBusyIndicator->hide();
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

void MainWindow::subWindowActivated(QMdiSubWindow* window)
{
   if (!window)
      return;
   if (WidgetDocument* document = dynamic_cast<WidgetDocument*>(window->widget()))
      document->activate();
}

void MainWindow::browsePeer(const Common::Hash& peerID)
{
   this->addWidgetBrowse(peerID);
}

void MainWindow::search(const QString& terms, bool ownFiles)
{
   this->addWidgetSearch(terms, ownFiles);
}

void MainWindow::roomJoined(const QString& name)
{
   this->addWidgetChatRoom(name);
}

void MainWindow::leaveRoom(QWidget* widgetChat)
{
   this->coreConnection->leaveRoom(static_cast<WidgetChat*>(widgetChat)->getRoomName());
}

/**
  * The widget can be a WidgetBrowse, WidgetSearch or WidgetChat (room).
  */
void MainWindow::removeWidget(QWidget* widget)
{
   if (WidgetBrowse* widgetBrowse = dynamic_cast<WidgetBrowse*>(widget))
      this->widgetsBrowse.removeOne(widgetBrowse);
   else if (WidgetSearch* widgetSearch = dynamic_cast<WidgetSearch*>(widget))
      this->widgetsSearch.removeOne(widgetSearch);
   else if (WidgetChat* widgetChat = dynamic_cast<WidgetChat*>(widget))
      this->widgetsChatRoom.removeOne(widgetChat);

   // TODO : what if a widget is removed without clicking the close button, is this button will ne remain undeleted!?
   //this->mdiAreaTabBar

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

void MainWindow::loadCustomStyle(const QString& filepath)
{
   QApplication* app = dynamic_cast<QApplication*>(QApplication::instance());

   if (!filepath.isNull())
   {
      // The css images are search from the current path.
      QDir::setCurrent(QCoreApplication::applicationDirPath());

      QFile file(filepath);
      if (file.open(QIODevice::ReadOnly))
      {
         this->customStyleLoaded = true;
         app->setStyleSheet(QString::fromUtf8(file.readAll()));
         this->ui->grip->setVisible(true);

         static const Qt::WindowFlags FRAMELESS_FLAGS = Qt::Window | Qt::FramelessWindowHint | Qt::WindowSystemMenuHint;
         if (this->windowFlags() != FRAMELESS_FLAGS)
         {
            this->setWindowFlags(FRAMELESS_FLAGS);
            this->resizeEvent(0);
            this->show();
         }
         return;
      }
      else
      {
         SETTINGS.set("style", QString(""));
         SETTINGS.save();
      }
   }

   // Set the default style.
   this->customStyleLoaded = false;
   app->setStyleSheet(QString());
   this->ui->grip->setVisible(false);
   this->setMask(QRegion());

   if (this->windowFlags() != this->initialWindowFlags)
   {
      this->setWindowFlags(this->initialWindowFlags);
      this->show();
   }
}

void MainWindow::maximize()
{
   if (this->windowState() & Qt::WindowMaximized)
   {
      this->showNormal();
   }
   else
   {
      this->showMaximized();
   }
}

void MainWindow::logEntireQWidgetTree()
{
   L_DEBU(Common::Global::getQObjectHierarchy(this));
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
         this->searchDock->setFocusToLineEdit();
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

void MainWindow::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
   {
      if (this->downloadsBusyIndicator)
         this->downloadsBusyIndicator->setToolTip(this->getBusyIndicatorToolTip());
      this->ui->retranslateUi(this);
   }
   else
      QWidget::changeEvent(event);
}

bool MainWindow::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->widgetChat && event->type() == QEvent::KeyPress)
   {
      this->keyPressEvent(static_cast<QKeyEvent*>(event));
   }
   else if (this->customStyleLoaded && obj == this->ui->grip)
   {
      if (event->type() == QEvent::MouseButtonPress && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton)
      {
         this->dragPosition = static_cast<QMouseEvent*>(event)->globalPos() - frameGeometry().topLeft();
      }
      if (event->type() == QEvent::MouseButtonRelease && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton)
      {
         this->dragPosition = QPoint();
      }
      else if (event->type() == QEvent::MouseMove && !this->isMaximized() && static_cast<QMouseEvent*>(event)->buttons() & Qt::LeftButton && !this->dragPosition.isNull())
      {
         move(static_cast<QMouseEvent*>(event)->globalPos() - this->dragPosition);
      }
      else if (event->type() == QEvent::Resize)
      {
         const QRegion maskedRegion(0, 0, this->ui->grip->width(), this->ui->grip->width());

         if (this->isMaximized())
            this->ui->grip->setMask(maskedRegion);
         else
         {
            const QRegion cornerTopRight = QRegion(this->ui->grip->width() - WINDOW_BORDER_RADIUS, 0, WINDOW_BORDER_RADIUS, WINDOW_BORDER_RADIUS).subtracted(QRegion(this->ui->grip->width() - 2 * WINDOW_BORDER_RADIUS, 0, 2 * WINDOW_BORDER_RADIUS, 2 * WINDOW_BORDER_RADIUS, QRegion::Ellipse));
            this->ui->grip->setMask(maskedRegion.subtracted(cornerTopRight));
         }
      }
      else if (event->type() == QEvent::MouseButtonDblClick && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton)
      {
         this->maximize();
      }
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

void MainWindow::resizeEvent(QResizeEvent* event)
{
   QMainWindow::resizeEvent(event);

   if (this->customStyleLoaded)
   {
      const QRegion maskedRegion(0, 0, this->width(), this->height());

      if (this->isMaximized())
         this->setMask(maskedRegion);
      else
      {
         const QRegion cornerTopLeft = QRegion(0, 0, WINDOW_BORDER_RADIUS, WINDOW_BORDER_RADIUS).subtracted(QRegion(0, 0, 2 * WINDOW_BORDER_RADIUS, 2 * WINDOW_BORDER_RADIUS, QRegion::Ellipse));
         const QRegion cornerTopRight = QRegion(this->width() - WINDOW_BORDER_RADIUS, 0, WINDOW_BORDER_RADIUS, WINDOW_BORDER_RADIUS).subtracted(QRegion(this->width() - 2 * WINDOW_BORDER_RADIUS, 0, 2 * WINDOW_BORDER_RADIUS, 2 * WINDOW_BORDER_RADIUS, QRegion::Ellipse));
         const QRegion cornerBottomLeft = QRegion(0, this->height() - WINDOW_BORDER_RADIUS, WINDOW_BORDER_RADIUS, WINDOW_BORDER_RADIUS).subtracted(QRegion(0, this->height() - 2 * WINDOW_BORDER_RADIUS, 2 * WINDOW_BORDER_RADIUS, 2 * WINDOW_BORDER_RADIUS, QRegion::Ellipse));
         const QRegion cornerBottomRight = QRegion(this->width() - WINDOW_BORDER_RADIUS, this->height() - WINDOW_BORDER_RADIUS, WINDOW_BORDER_RADIUS, WINDOW_BORDER_RADIUS).subtracted(QRegion(this->width() - 2 * WINDOW_BORDER_RADIUS, this->height() - 2 * WINDOW_BORDER_RADIUS, 2 * WINDOW_BORDER_RADIUS, 2 * WINDOW_BORDER_RADIUS, QRegion::Ellipse));

         this->setMask(maskedRegion.subtracted(cornerTopLeft).subtracted(cornerTopRight).subtracted(cornerBottomLeft).subtracted(cornerBottomRight));
      }
   }
}

#ifdef Q_OS_WIN32
   void MainWindow::showEvent(QShowEvent* /*event*/)
   {
      // It seems that the handle change every time the style is changed.
      this->taskbar.setWinHandle(this->winId());
   }

   bool MainWindow::winEvent(MSG* message, long* result)
   {
      this->taskbar.winEvent(message, result);
      return false;
   }
#endif

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

   this->ui->mdiArea->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
}

void MainWindow::setApplicationStateAsDisconnected()
{
   this->taskbar.setStatus(TaskbarButtonStatus::BUTTON_STATUS_NOPROGRESS);
   this->removeWidgetUploads();
   this->removeWidgetDownloads();
   this->removeWidgetChat();
   this->removeAllWidgets();
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
      state = QByteArray::fromHex("000000ff00000000fd0000000200000000000000dd00000251fc0200000006fb000000120064006f0063006b005000650065007200730100000020000001970000000000000000fb000000140064006f0063006b00530065006100720063006801000000000000001c0000000000000000fb000000140053006500610072006300680044006f0063006b01000000000000003c0000003c00fffffffb00000012005000650065007200730044006f0063006b0100000040000001760000004b00fffffffb000000120064006f0063006b0052006f006f006d0073010000019d000000ba0000000000000000fb000000120052006f006f006d00730044006f0063006b01000001ba000000970000006500ffffff00000003000003a30000005dfc0100000001fb0000000e0064006f0063006b004c006f00670000000000000003a30000006100ffffff000003d10000025100000004000000040000000800000008fc00000000");
   this->restoreState(state);
}

QString MainWindow::getBusyIndicatorToolTip() const
{
   return tr("Waiting the cache loading process is finished before loading the download queue");
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
   connect(this->widgetSettings, SIGNAL(languageChanged(QString)), this, SIGNAL(languageChanged(QString)));
   connect(this->widgetSettings, SIGNAL(styleChanged(QString)), this, SLOT(loadCustomStyle(QString)));
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
   if (this->widgetChat)
      return;

   this->widgetChat = new WidgetChat(this->coreConnection, this->peersDock->getModel(), this);
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
   if (this->widgetDownloads)
      return;

   this->widgetDownloads = new WidgetDownloads(this->coreConnection, this->peersDock->getModel(), this->sharedDirsModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetDownloads, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_DOWNLOAD);
   this->widgetDownloads->setWindowState(Qt::WindowMaximized);

   connect(this->widgetDownloads, SIGNAL(globalProgressChanged(quint64, quint64)), this, SLOT(onGlobalProgressChanged(quint64, quint64)));

   this->downloadsBusyIndicator = new BusyIndicator();
   this->downloadsBusyIndicator->setObjectName("tabWidget");
   this->downloadsBusyIndicator->setToolTip(this->getBusyIndicatorToolTip());
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, this->downloadsBusyIndicator);
}

void MainWindow::removeWidgetDownloads()
{
   if (this->widgetDownloads)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetDownloads->parent()));
      this->widgetDownloads = 0;
      this->downloadsBusyIndicator = 0;
   }
}

void MainWindow::addWidgetUploads()
{
   if (this->widgetUploads)
      return;

   this->widgetUploads = new WidgetUploads(this->coreConnection, this->peersDock->getModel(), this);
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

   WidgetBrowse* widgetBrowse = new WidgetBrowse(this->coreConnection, this->peersDock->getModel(), this->sharedDirsModel, peerID, this);
   this->ui->mdiArea->addSubWindow(widgetBrowse, Qt::CustomizeWindowHint);
   widgetBrowse->setWindowState(Qt::WindowMaximized);
   this->widgetsBrowse << widgetBrowse;

   QWidget* buttons = new QWidget();
   buttons->setObjectName("tabWidget");

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
   WidgetSearch* widgetSearch = new WidgetSearch(this->coreConnection, this->peersDock->getModel(), this->sharedDirsModel, term, searchInOwnFiles, this);
   this->ui->mdiArea->addSubWindow(widgetSearch, Qt::CustomizeWindowHint);
   widgetSearch->setWindowState(Qt::WindowMaximized);
   this->widgetsSearch << widgetSearch;
   connect(widgetSearch, SIGNAL(browse(const Common::Hash&, const Protos::Common::Entry&)), this, SLOT(addWidgetBrowse(const Common::Hash&, const Protos::Common::Entry&)));

   TabCloseButton* closeButton = new TabCloseButton(widgetSearch);
   closeButton->setObjectName("tabWidget");
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, closeButton);

   return widgetSearch;
}

WidgetChat* MainWindow::addWidgetChatRoom(const QString& roomName, bool switchTo)
{
   // If the chat room is already open.
   for (QListIterator<WidgetChat*> i(this->widgetsChatRoom); i.hasNext();)
   {
      WidgetChat* chatRoom = i.next();
      if (chatRoom->getRoomName() == roomName)
      {
         if (switchTo)
            this->ui->mdiArea->setActiveSubWindow(static_cast<QMdiSubWindow*>(chatRoom->parent()));
         return chatRoom;
      }
   }

   QMdiSubWindow* currentWindow = this->ui->mdiArea->currentSubWindow();

   WidgetChat* widgetChat = new WidgetChat(this->coreConnection, this->peersDock->getModel(), roomName, this);
   widgetChat->installEventFilterOnInput(this);
   this->ui->mdiArea->addSubWindow(widgetChat, Qt::CustomizeWindowHint);
   widgetChat->setWindowState(Qt::WindowMaximized);
   this->widgetsChatRoom << widgetChat;

   TabCloseButton* closeButton = new TabCloseButton(widgetChat, nullptr, false);
   closeButton->setObjectName("tabWidget");
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(leaveRoom(QWidget*)));
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, closeButton);

   if (!switchTo && currentWindow)
      this->ui->mdiArea->setActiveSubWindow(currentWindow);

   return widgetChat;
}

void MainWindow::removeAllWidgets()
{
   foreach (WidgetBrowse* widget, this->widgetsBrowse)
      this->removeWidget(widget);

   foreach (WidgetSearch* widget, this->widgetsSearch)
      this->removeWidget(widget);

   foreach (WidgetChat* widget, this->widgetsChatRoom)
      this->removeWidget(widget);
}
