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

#include <StatusBar.h>
#include <Log.h>

MainWindow::MainWindow(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QMainWindow(parent),
   coreConnection(coreConnection),
   ui(new Ui::MainWindow),
   searchDock(new SearchDock(this->coreConnection, this)),
   peersDock(new PeersDock(this->coreConnection, this)),
   roomsDock(new RoomsDock(this->coreConnection, this)),
   customStyleLoaded(false),
   logAutoScroll(true),
   logModel(coreConnection)
{
   this->ui->setupUi(this);

   this->taskbar.setStatus(TaskbarButtonStatus::BUTTON_STATUS_NOPROGRESS);

   this->mdiArea = new MdiArea(this->coreConnection, this->peersDock->getModel(), this->taskbar, this->ui->centralWidget);
   /*QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
   sizePolicy.setHorizontalStretch(0);
   sizePolicy.setVerticalStretch(0);
   sizePolicy.setHeightForWidth(this->mdiArea->sizePolicy().hasHeightForWidth());
   this->mdiArea->setSizePolicy(sizePolicy);*/
   this->ui->verticalLayout->addWidget(this->mdiArea);
   connect(this->mdiArea, SIGNAL(languageChanged(QString)), this, SIGNAL(languageChanged(QString)));
   connect(this->mdiArea, SIGNAL(styleChanged(QString)), this, SLOT(loadCustomStyle(QString)));

   this->initialWindowFlags = this->windowFlags();

   StatusBar* statusBar = new StatusBar(this->coreConnection);
   ui->statusBar->addWidget(statusBar, 1);
   connect(statusBar, SIGNAL(showDockLog(bool)), this->ui->dockLog, SLOT(setVisible(bool)));
   connect(statusBar, SIGNAL(downloadClicked()), this->mdiArea, SLOT(showDownloads()));
   connect(statusBar, SIGNAL(uploadClicked()), this->mdiArea, SLOT(showUploads()));

   ///// Dockable widgets
   this->addDockWidget(Qt::LeftDockWidgetArea, this->searchDock);
   connect(this->searchDock, SIGNAL(search(const Protos::Common::FindPattern&, bool)), this, SLOT(search(const Protos::Common::FindPattern&, bool)));
   this->addDockWidget(Qt::LeftDockWidgetArea, this->peersDock);
   connect(this->peersDock, SIGNAL(browsePeer(Common::Hash)), this, SLOT(browsePeer(Common::Hash)));
   this->addDockWidget(Qt::LeftDockWidgetArea, this->roomsDock);
   connect(this->roomsDock, SIGNAL(roomJoined(QString)), this, SLOT(roomJoined(QString)));
   /////

   this->ui->tblLog->setModel(&this->logModel);
   this->ui->tblLog->setItemDelegate(&this->logDelegate);
   this->ui->tblLog->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->tblLog->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
   this->ui->tblLog->horizontalHeader()->setVisible(false);
   this->ui->tblLog->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
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

   this->ui->grip->setVisible(false);
   this->ui->grip->installEventFilter(this);
   connect(this->ui->butClose, SIGNAL(clicked()), this, SLOT(close()));
   connect(this->ui->butMinimize, SIGNAL(clicked()), this, SLOT(showMinimized()));
   connect(this->ui->butMaximize, SIGNAL(clicked()), this, SLOT(maximize()));
   if (!SETTINGS.get<QString>("style").isEmpty())
      this->loadCustomStyle(QCoreApplication::applicationDirPath() % "/" % Common::Constants::STYLE_DIRECTORY % "/" % SETTINGS.get<QString>("style") % "/" % Common::Constants::STYLE_FILE_NAME);

   this->restoreWindowsSettings();

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

   delete this->ui;
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
   msgBox.setText(QString("<p>%1</p><p>%2 <em>%3:%4</em></p>").arg(error).arg(tr("Core address:")).arg(this->coreConnection->getConnectionInfoConnecting().address).arg(this->coreConnection->getConnectionInfoConnecting().port));
   msgBox.setIcon(QMessageBox::Information);
   msgBox.setStandardButtons(QMessageBox::Ok);
   msgBox.exec();
}

void MainWindow::coreConnected()
{
   L_USER(tr("Connected to the core"));
}

void MainWindow::coreDisconnected(bool forced)
{
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

void MainWindow::browsePeer(const Common::Hash& peerID)
{
   this->mdiArea->openBrowseWindow(peerID);
}

void MainWindow::search(const Protos::Common::FindPattern& findPattern, bool local)
{
   this->mdiArea->openSearchWindow(findPattern, local);
}

void MainWindow::roomJoined(const QString& name)
{
   this->mdiArea->openChatWindow(name);
}

void MainWindow::logScrollChanged(int value)
{
   this->logAutoScroll = value == this->ui->tblLog->verticalScrollBar()->maximum();
}

void MainWindow::newLogMessage()
{
   if (this->logAutoScroll)
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
      // Search.
      case 'f':
      case 'F':
         this->searchDock->setFocusToLineEdit();
         event->accept();
         return;

      // Close the current window.
      case 'w':
      case 'W':
         this->mdiArea->closeCurrentWindow();
         event->accept();
         return;
      }
   }
   // ALT.
   else if (event->modifiers().testFlag(Qt::AltModifier))
   {
      // Focus the nth window.
      if (event->key() >= '1' && event->key() <= '9')
         this->mdiArea->focusNthWindow(event->key() - '1');
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
      this->ui->retranslateUi(this);

   QMainWindow::changeEvent(event);
}

bool MainWindow::eventFilter(QObject* obj, QEvent* event)
{
   if (this->customStyleLoaded && obj == this->ui->grip)
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
      this->taskbar.setWinHandle((HWND)this->winId());
   }

   bool MainWindow::nativeEvent(const QByteArray&, void* message, long* result)
   {
      this->taskbar.winEvent(reinterpret_cast<MSG*>(message), result);
      return false;
   }
#endif

void MainWindow::saveWindowsSettings()
{
   L_DEBU(QString("Save state : %1").arg(QString::fromLatin1(this->saveState().toHex().data())));

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
      state = QByteArray::fromHex("000000ff00000000fd0000000200000000000000df0000020efc0200000005fb000000120064006f0063006b005000650065007200730100000020000001970000000000000000fb000000140064006f0063006b00530065006100720063006801000000000000001c0000000000000000fb000000120064006f0063006b0052006f006f006d0073010000019d000000ba0000000000000000fc00000000000001ce0000007a01000014fa000000000100000002fb00000012005000650065007200730044006f0063006b0100000000ffffffff0000006100fffffffb000000120052006f006f006d00730044006f0063006b0100000000ffffffff0000006f00fffffffb000000140053006500610072006300680044006f0063006b01000001d20000003c0000003c00ffffff00000003000003a30000005dfc0100000001fb0000000e0064006f0063006b004c006f00670000000000000003a30000006100ffffff0000039d0000020e00000004000000040000000800000008fc00000000");
   this->restoreState(state);
}
