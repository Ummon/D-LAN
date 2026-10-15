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
#include <QScreen>
#include <QWindow>

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/RemoteCoreController/Builder.h>

#include <D-LAN_GUI.h>
#include <WindowFrame.h>
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
   windowBorderRadius(DEFAULT_WINDOW_BORDER_RADIUS),
   windowFrameWidth(0),
   titleBarOverDocks(false),
   titleBarSpacing(DEFAULT_TITLE_BAR_SPACING),
   dialogFrameWidth(0),
   ignoreScreenScaling(false),
   dialogDecorator(new DialogDecorator(this)),
   logAutoScroll(true),
   logModel(coreConnection)
{
   // Set inactive style as active one to avoid some widgets to become grey when the window doesn't have the focus.
   QPalette pal = this->palette();
   pal.setColor(QPalette::Inactive, QPalette::Highlight, pal.color(QPalette::Active, QPalette::Highlight));
   pal.setColor(QPalette::Inactive, QPalette::HighlightedText, pal.color(QPalette::Active, QPalette::HighlightedText));
   this->setPalette(pal);

   this->ui->setupUi(this);

   this->setTabPosition(Qt::AllDockWidgetAreas, QTabWidget::North);

   this->taskbar.setStatus(TaskbarButtonStatus::BUTTON_STATUS_NOPROGRESS);

   this->mdiArea = new MdiArea(this->coreConnection, this->peersDock->getModel(), this->taskbar, this->ui->centralWidget);
   this->ui->verticalLayout->addWidget(this->mdiArea);
   connect(this->mdiArea, &MdiArea::languageChanged, this, &MainWindow::languageChanged);
   connect(this->mdiArea, &MdiArea::styleChanged, this, &MainWindow::loadCustomStyle);
   connect(this, &MainWindow::windowDecorationChanged, this, &MainWindow::updateWindowDecoration);

   this->initialWindowFlags = this->windowFlags();

   StatusBar* statusBar = new StatusBar(this->coreConnection);
   ui->statusBar->addWidget(statusBar, 1);
   connect(statusBar, &StatusBar::showDockLog, this->ui->dockLog, &QDockWidget::setVisible);
   connect(statusBar, &StatusBar::downloadClicked, this->mdiArea, &MdiArea::showDownloads);
   connect(statusBar, &StatusBar::uploadClicked, this->mdiArea, &MdiArea::showUploads);
   connect(statusBar, &StatusBar::connectToLocalClicked, this->mdiArea, &MdiArea::connectToLocalCore);

   ///// Dockable widgets
   this->addDockWidget(Qt::LeftDockWidgetArea, this->searchDock);
   connect(
      this->searchDock,
      qOverload<const Protos::Common::FindPattern&, bool>(&SearchDock::search),
      this->mdiArea,
      &MdiArea::openSearchWindow
   );
   this->addDockWidget(Qt::LeftDockWidgetArea, this->peersDock);
   connect(this->peersDock, &PeersDock::browsePeer, this->mdiArea, &MdiArea::openBrowseWindow);
   this->addDockWidget(Qt::LeftDockWidgetArea, this->roomsDock);
   connect(this->roomsDock, &RoomsDock::roomJoined, this->mdiArea, &MdiArea::openChatWindow);
   /////

   this->ui->tblLog->setModel(&this->logModel);
   this->ui->tblLog->setItemDelegate(&this->logDelegate);
   this->ui->tblLog->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->tblLog->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
   this->ui->tblLog->horizontalHeader()->setVisible(false);
   this->ui->tblLog->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
   this->ui->tblLog->verticalHeader()->setDefaultSectionSize(QFontMetrics(QApplication::font()).height() + 2);
   this->ui->tblLog->verticalHeader()->setVisible(false);
   this->ui->tblLog->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblLog->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblLog->setShowGrid(false);
   // If we didn't set auto-scroll to 'false', when the selection is on the first item and it being deleted,
   // the selection automatically change to the next item and the view scroll to it.
   this->ui->tblLog->setAutoScroll(false);
   this->ui->tblLog->setAlternatingRowColors(true);

   connect(&this->logModel, &LogModel::rowsInserted, this, &MainWindow::newLogMessage);
   connect(this->ui->tblLog->verticalScrollBar(), &QScrollBar::valueChanged, this, &MainWindow::logScrollChanged);
   connect(this->ui->dockLog, &QDockWidget::visibilityChanged, statusBar, &StatusBar::dockLogVisibilityChanged);

   this->ui->grip->setVisible(false);
   this->ui->grip->installEventFilter(this);
   new WindowFrame(this);
   connect(this->ui->butClose, &QPushButton::clicked, this, &MainWindow::close);
   connect(this->ui->butMinimize, &QPushButton::clicked, this, &MainWindow::showMinimized);
   connect(this->ui->butMaximize, &QPushButton::clicked, this, &MainWindow::maximize);

   // Create the native window with its normal frame before a custom style removes it. On Windows, a window created
   // frameless keeps a client area covering the whole window when the frame is restored later (going back to
   // the default style): the title bar and the borders would stay invisible.
   this->winId();

   this->loadCustomStyle(SETTINGS.get<QString>("style"));

   this->restoreWindowsSettings();

   connect(this->coreConnection.data(), &RCC::ICoreConnection::connectingError, this, &MainWindow::coreConnectionError);
   connect(this->coreConnection.data(), &RCC::ICoreConnection::connected, this, &MainWindow::coreConnected);
   connect(this->coreConnection.data(), &RCC::ICoreConnection::disconnected, this, &MainWindow::coreDisconnected);

   // The password saved by a version prior to 1.4.2 is still accepted, see 'Common::SaltedPassword'.
   Common::SaltedPassword password = Common::SaltedPassword::fromStr(SETTINGS.get<QString>("core_password"));
   if (password.isNull() && !password.isLegacy())
      password.legacyHash = SETTINGS.get<Common::Hash>("password");
   this->coreConnection->connectToCore(SETTINGS.get<QString>("core_address"), SETTINGS.get<quint32>("core_port"), password);

#ifdef DEBUG
   QPushButton* logEntireQWidgetTreeButton = new QPushButton();
   logEntireQWidgetTreeButton->setText("log widget tree");
   connect(logEntireQWidgetTreeButton, &QPushButton::clicked, this, &MainWindow::logEntireQWidgetTree);
   this->ui->statusBar->addWidget(logEntireQWidgetTreeButton);
#endif
}

MainWindow::~MainWindow()
{
   this->saveWindowsSettings();

   this->coreConnection->disconnect(this); // Disconnect all signals.
   this->logModel.disconnect(this);

   // The windows of the MDI area hold a reference to the peer list model owned by 'peersDock'. The docks are the
   // first children of this window, 'QWidget' would delete them before the MDI area.
   delete this->mdiArea;

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
      error = tr("The host is unknown");
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
   case RCC::ICoreConnection::RCC_ERROR_TLS:
      error = tr("Unable to establish a trusted encrypted connection. The Core's certificate may have changed or TLS may be unavailable. See the log for details.");
      break;
   case RCC::ICoreConnection::RCC_ERROR_CLOSED_BY_CORE:
      error = tr("The core closed the connection. Remote access requires a password to be defined on the core. The core may also have too many connections.");
      break;
   case RCC::ICoreConnection::RCC_ERROR_CORE_NOT_AUTHENTICATED:
      error = tr("The core couldn't prove that it knows the password. The connection may have been intercepted.");
      break;
   case RCC::ICoreConnection::RCC_ERROR_INCOMPATIBLE_VERSION:
      error = tr("The versions of the core and of this GUI are incompatible for remote control. Update them to the same version.");
      break;
   case RCC::ICoreConnection::RCC_ERROR_UNKNOWN:
      error = tr("Error unknown");
   }

   // The main window can be destroyed while the dialog is open (for example via tray Exit).
   auto* msgBox = new QMessageBox(this);
   msgBox->setAttribute(Qt::WA_DeleteOnClose);
   msgBox->setWindowTitle(tr("Unable to connect to the core"));
   msgBox->setText(
      QString("<p>%1</p><p>%2 <em>%3:%4</em></p>")
         .arg(error, tr("Core address:"), this->coreConnection->getConnectionInfoConnecting().address)
         .arg(this->coreConnection->getConnectionInfoConnecting().port)
   );
   msgBox->setIcon(QMessageBox::Information);
   msgBox->setStandardButtons(QMessageBox::Ok);
   msgBox->open();
}

void MainWindow::coreConnected()
{
   L_USER(tr("Connected to the core"));
}

void MainWindow::coreDisconnected(bool forced)
{
   if (!forced && !this->coreConnection->isConnecting())
   {
      auto* msgBox = new QMessageBox(this);
      msgBox->setAttribute(Qt::WA_DeleteOnClose);
      msgBox->setWindowTitle(tr("Connection lost"));
      msgBox->setText(
         QString("<p>%1</p><p>%2 <em>%3:%4</em></p>")
            .arg(
               tr("The connection to the core has been lost"),
               tr("Core address:"),
               this->coreConnection->getConnectionInfo().address
            )
            .arg(this->coreConnection->getConnectionInfo().port)
      );
      msgBox->setIcon(QMessageBox::Information);
      msgBox->setStandardButtons(QMessageBox::Ok);
      msgBox->open();
   }
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

/**
  * @param styleName The name of the directory of the style, the default style is loaded if it's empty.
  */
void MainWindow::loadCustomStyle(const QString& styleName)
{
   QApplication* app = dynamic_cast<QApplication*>(QApplication::instance());
   const bool wasVisible = this->isVisible();

   // A style only sets the properties of the decoration it changes, they are set when the window is polished.
   this->windowBorderRadius = DEFAULT_WINDOW_BORDER_RADIUS;
   this->windowFrameWidth = 0;
   this->titleBarOverDocks = false;
   this->titleBarSpacing = DEFAULT_TITLE_BAR_SPACING;
   this->dialogFrameWidth = 0;
   this->ignoreScreenScaling = false;

   if (!styleName.isEmpty())
   {
      // The css images are search from the current path.
      QDir::setCurrent(Common::Global::getResourceFolder());

      const QString filepath =
         Common::Global::getResourceFolder() % "/" %
         Common::Constants::STYLE_DIRECTORY % "/" %
         styleName % "/" % Common::Constants::STYLE_FILE_NAME;

      QFile file(filepath);
      if (file.open(QIODevice::ReadOnly))
      {
         this->customStyleLoaded = true;
         app->setStyleSheet(QString::fromUtf8(file.readAll()));
         this->ensurePolished(); // To have the properties set by the style, the window isn't polished yet when it's created.
         this->updateScreenScalingSetting(wasVisible);
         this->ui->grip->setVisible(true);

         static const Qt::WindowFlags FRAMELESS_FLAGS = Qt::Window | Qt::FramelessWindowHint | Qt::WindowSystemMenuHint;
         if (this->windowFlags() != FRAMELESS_FLAGS)
         {
            this->setWindowFlags(FRAMELESS_FLAGS);
            this->resizeEvent(nullptr);
            if (wasVisible)
               this->show();
         }
         this->updateWindowDecoration();
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
   // We use the fusion style instead of the "native" one because some widget doesn't render correctly in native.
   // For example QProgressBar cannot render text in it.
   app->setStyle("Fusion");
   app->setStyleSheet(QString());
   this->updateScreenScalingSetting(wasVisible);
   this->ui->grip->setVisible(false);
   this->updateWindowDecoration();

   if (this->windowFlags() != this->initialWindowFlags)
   {
      this->setWindowFlags(this->initialWindowFlags);
      if (wasVisible)
         this->show();
   }
}

/**
  * The scaling of the screen can only be ignored from the start of the GUI (see 'main.cpp'): the choice of the style is
  * saved for the next start, and a restart is asked when it doesn't match the way the GUI has been started.
  */
void MainWindow::updateScreenScalingSetting(bool askForRestart)
{
   if (SETTINGS.get<bool>("ignore_screen_scaling") != this->ignoreScreenScaling)
   {
      SETTINGS.set("ignore_screen_scaling", this->ignoreScreenScaling);
      SETTINGS.save();
   }

   if (askForRestart && this->ignoreScreenScaling != D_LAN_GUI::isScreenScalingIgnored())
   {
      auto* msgBox = new QMessageBox(this);
      msgBox->setAttribute(Qt::WA_DeleteOnClose);
      msgBox->setWindowTitle(tr("Restart needed"));
      msgBox->setText(tr("D-LAN must be restarted to display this style correctly."));
      msgBox->setIcon(QMessageBox::Information);
      msgBox->setStandardButtons(QMessageBox::Ok);
      msgBox->open();
   }
}

void MainWindow::updateWindowDecoration()
{
   QWidget* titleBar = this->ui->grip;
   if (this->titleBarOverDocks != (this->menuWidget() == titleBar))
   {
      const bool visible = !titleBar->isHidden();
      if (this->titleBarOverDocks)
         this->setMenuWidget(titleBar);
      else // Taking it back from the menu bar area unsets it there, 'setMenuWidget(nullptr)' would delete it.
         this->ui->verticalLayout->insertWidget(0, titleBar);
      titleBar->setVisible(visible);
   }
   this->ui->horizontalLayout->setContentsMargins(this->titleBarSpacing, this->titleBarSpacing, this->titleBarSpacing, this->titleBarSpacing);
   this->ui->horizontalLayout->setSpacing(this->titleBarSpacing);

   // As with a native frame, there is no frame when the window is maximized.
   const int frameWidth = this->customStyleLoaded && !this->isMaximized() && !this->isFullScreen() ? this->windowFrameWidth : 0;
   this->setContentsMargins(frameWidth, frameWidth, frameWidth, frameWidth);
   this->setMouseTracking(frameWidth > 0); // For the cursor over the frame, see 'WindowFrame'.

   this->dialogDecorator->setFrameWidths(this->customStyleLoaded ? this->dialogFrameWidth : 0, this->windowFrameWidth);

   this->updateWindowMask();
   if (this->customStyleLoaded)
      this->updateTitleBarMask();
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
      case Qt::Key_F:
         this->searchDock->setFocusToLineEdit();
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
   event->accept();
   this->hide();
   this->deleteLater();
}

void MainWindow::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);
   else if (event->type() == QEvent::WindowStateChange)
   {
      this->updateWindowDecoration();

      // A style can show the maximize button differently when the window is maximized
      // ('GUI--MainWindow[maximized="true"] #butMaximize'), Qt only evaluates such a selector when the button is polished.
      if (this->customStyleLoaded)
      {
         this->style()->unpolish(this->ui->butMaximize);
         this->style()->polish(this->ui->butMaximize);
         this->ui->butMaximize->update();
      }
   }

   QMainWindow::changeEvent(event);
}

bool MainWindow::eventFilter(QObject* obj, QEvent* event)
{
   if (this->customStyleLoaded && obj == this->ui->grip)
   {
      if (event->type() == QEvent::MouseButtonPress && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton)
      {
         this->dragPosition = static_cast<QMouseEvent*>(event)->globalPosition().toPoint() - frameGeometry().topLeft();
      }
      if (event->type() == QEvent::MouseButtonRelease && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton)
      {
         this->dragPosition = QPoint();
      }
      else if (event->type() == QEvent::MouseMove && !this->isMaximized() && static_cast<QMouseEvent*>(event)->buttons() & Qt::LeftButton && !this->dragPosition.isNull())
      {
         // Let the window manager do the move when possible: on Wayland a client isn't allowed to set its own position, 'move(..)' has no effect.
         // The move is started here and not when the button is pressed to not break the double-click.
         // The button release will not be received during a system move, thus the drag has to be ended now.
         if (this->windowHandle() && this->windowHandle()->startSystemMove())
            this->dragPosition = QPoint();
         else
            move(static_cast<QMouseEvent*>(event)->globalPosition().toPoint() - this->dragPosition);
      }
      else if (event->type() == QEvent::Resize)
      {
         this->updateTitleBarMask();
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
   this->updateWindowMask();
}

void MainWindow::updateWindowMask()
{
   if (!this->customStyleLoaded)
   {
      this->clearMask();
      return;
   }

   const int radius = this->windowBorderRadius;
   const QRegion maskedRegion(0, 0, this->width(), this->height());

   if (this->isMaximized() || radius <= 0)
      this->setMask(maskedRegion);
   else
   {
      const QRegion cornerTopLeft = QRegion(0, 0, radius, radius).subtracted(QRegion(0, 0, 2 * radius, 2 * radius, QRegion::Ellipse));
      const QRegion cornerTopRight = QRegion(this->width() - radius, 0, radius, radius).subtracted(QRegion(this->width() - 2 * radius, 0, 2 * radius, 2 * radius, QRegion::Ellipse));
      const QRegion cornerBottomLeft = QRegion(0, this->height() - radius, radius, radius).subtracted(QRegion(0, this->height() - 2 * radius, 2 * radius, 2 * radius, QRegion::Ellipse));
      const QRegion cornerBottomRight = QRegion(this->width() - radius, this->height() - radius, radius, radius).subtracted(QRegion(this->width() - 2 * radius, this->height() - 2 * radius, 2 * radius, 2 * radius, QRegion::Ellipse));

      this->setMask(maskedRegion.subtracted(cornerTopLeft).subtracted(cornerTopRight).subtracted(cornerBottomLeft).subtracted(cornerBottomRight));
   }
}

void MainWindow::updateTitleBarMask()
{
   const int radius = this->windowBorderRadius;
   const QRegion maskedRegion(0, 0, this->ui->grip->width(), this->ui->grip->width());

   if (this->isMaximized() || radius <= 0)
      this->ui->grip->setMask(maskedRegion);
   else
   {
      const QRegion cornerTopRight = QRegion(this->ui->grip->width() - radius, 0, radius, radius).subtracted(QRegion(this->ui->grip->width() - 2 * radius, 0, 2 * radius, 2 * radius, QRegion::Ellipse));
      this->ui->grip->setMask(maskedRegion.subtracted(cornerTopRight));
   }
}

#ifdef Q_OS_WIN32
   void MainWindow::showEvent(QShowEvent* /*event*/)
   {
      // It seems that the handle change every time the style is changed.
      this->taskbar.setWinHandle((HWND)this->winId());
   }

   bool MainWindow::nativeEvent(const QByteArray&, void* message, qintptr* result)
   {
      this->taskbar.winEvent(reinterpret_cast<MSG*>(message), result);
      return false;
   }
#endif

void MainWindow::saveWindowsSettings()
{
   L_DEBU(QString("Save state: %1").arg(QString::fromLatin1(this->saveState().toHex().data())));

   SETTINGS.set("windows_state", this->saveState());

   SETTINGS.set("main_window_maximized", this->isMaximized());
   if (!this->isMaximized())
   {
      SETTINGS.set("main_window_width", static_cast<quint32>(this->size().width()));
      SETTINGS.set("main_window_height", static_cast<quint32>(this->size().height()));
   }

   SETTINGS.save();
}

void MainWindow::restoreWindowsSettings()
{
   this->resize(QSize(SETTINGS.get<quint32>("main_window_width"), SETTINGS.get<quint32>("main_window_height")));

   QByteArray state = SETTINGS.get<QByteArray>("windows_state");
   if (state.isEmpty())
      state = QByteArray::fromHex("000000ff00000000fd000000020000000000000116000002bcfc0200000003fb000000140053006500610072006300680044006f0063006b0100000000000000250000002500000025fb00000012005000650065007200730044006f0063006b010000002b000001c30000004a00fffffffb000000120052006f006f006d00730044006f0063006b01000001f4000000c80000006400ffffff000000030000058f0000009cfc0100000001fb0000000e0064006f0063006b004c006f006708000000000000058f0000005c00ffffff00000428000002bc00000004000000040000000800000008fc00000000");
   this->restoreState(state);

   // Choose the screen and position before showing: automatic placement can
   // otherwise use the monitor containing the cursor.
   if (QScreen* screen = QGuiApplication::primaryScreen())
   {
      this->setScreen(screen);
      const QRect available = screen->availableGeometry();
      this->resize(this->size().boundedTo(available.size()));
      this->move(available.topLeft() + QPoint(
         qMax(0, (available.width() - this->frameGeometry().width()) / 2),
         qMax(0, (available.height() - this->frameGeometry().height()) / 2)));
   }

   if (SETTINGS.get<bool>("main_window_maximized"))
      this->setWindowState(this->windowState() | Qt::WindowMaximized);
}
