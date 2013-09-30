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
  
#include <MDI/MdiArea.h>
using namespace GUI;

#include <QMdiSubWindow>
#include <QCoreApplication>
#include <QStringBuilder>

#include <Common/Settings.h>

#include <Log.h>
#include <Constants.h>
#include <Utils.h>
#include <MDI/MdiWidget.h>
#include <MDI/TabButtons.h>

MdiArea::MdiArea(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, Taskbar& taskbar, QWidget* parent) :
   QMdiArea(parent),
   emoticon(Utils::emoticonsDirectoryPath(), Constants::DEFAULT_EMOTICON_THEME),
   coreConnection(coreConnection),
   peerListModel(peerListModel),
   taskbar(taskbar),
   settingsWidget(nullptr),
   chatWidget(nullptr),
   downloadsWidget(nullptr),
   uploadsWidget(nullptr),
   downloadsBusyIndicator(nullptr)
{
   this->setObjectName("mdiArea");
   /*sizePolicy.setHeightForWidth(mdiArea->sizePolicy().hasHeightForWidth());
   mdiArea->setSizePolicy(sizePolicy);*/
   this->setActivationOrder(QMdiArea::ActivationHistoryOrder);
   this->setViewMode(QMdiArea::TabbedView);
   this->setDocumentMode(true);
   this->setOption(QMdiArea::DontMaximizeSubWindowOnActivation, true);

   connect(this, SIGNAL(subWindowActivated(QMdiSubWindow*)), this, SLOT(subWindowActivated(QMdiSubWindow*)));

   this->mdiAreaTabBar = this->findChild<QTabBar*>();
   this->mdiAreaTabBar->setMovable(true);
   this->mdiAreaTabBar->installEventFilter(this);
   connect(this->mdiAreaTabBar, SIGNAL(tabMoved(int, int)), this, SLOT(tabMoved(int, int)));

   this->addSettingsWindow();

   connect(this->coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreDisconnected(false); // Initial state.
}

MdiArea::~MdiArea()
{
   this->removeSettingsWindow();
}

void MdiArea::focusNthWindow(int num)
{
   if (num < this->subWindowList().size())
      this->setActiveSubWindow(this->subWindowList()[num]);
}

/**
  * Called when the user explicitely wants to close the current window.
  */
void MdiArea::closeCurrentWindow()
{
   if (this->currentSubWindow())
   {
      QWidget* widget = this->currentSubWindow()->widget();

      if (dynamic_cast<BrowseWidget*>(widget) || dynamic_cast<SearchWidget*>(widget))
         this->removeWidget(widget);
      else if (dynamic_cast<ChatWidget*>(widget) && !dynamic_cast<ChatWidget*>(widget)->isGeneral())
         this->leaveRoom(widget);
   }
}

void MdiArea::openBrowseWindow(const Hash& peerID)
{
   this->addBrowseWindow(peerID);
}

void MdiArea::openSearchWindow(const QString& terms, bool ownFiles)
{
   this->addSearchWindow(terms, ownFiles);
}

void MdiArea::openChatWindow(const QString& roomName)
{
   if (!roomName.isEmpty())
   {
      this->addChatWindow(roomName);
      this->newOpenedChatRoom = roomName;
   }
}

void MdiArea::showDownloads()
{
   this->setActiveSubWindow(static_cast<QMdiSubWindow*>(this->downloadsWidget->parent()));
}

void MdiArea::showUploads()
{
   this->setActiveSubWindow(static_cast<QMdiSubWindow*>(this->uploadsWidget->parent()));
}

void MdiArea::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
   {
      if (this->downloadsBusyIndicator)
         this->downloadsBusyIndicator->setToolTip(this->getBusyIndicatorToolTip());
   }
   else
      QWidget::changeEvent(event);
}

bool MdiArea::eventFilter(QObject* obj, QEvent* event)
{
   if // Prohibits the user to close tab with the middle button.
   (
      obj == this->mdiAreaTabBar &&
      (event->type() == QEvent::MouseButtonPress || event->type() == QEvent::MouseButtonDblClick) &&
      static_cast<QMouseEvent*>(event)->button() == Qt::MiddleButton
   )
      return true;

   return QMdiArea::eventFilter(obj, event);
}

void MdiArea::newState(const Protos::GUI::State& state)
{
   // Synchronize the joined chat rooms with the opened windows.
   // There is two reasons for doing that:
   //  1) The joined room may be persisted by the core.
   //  2) Another GUI connected to the same core may open or close a chat room.
   QSet<QString> joinedRooms;
   for (int i = 0; i < state.rooms_size(); i++)
      if (state.rooms(i).joined())
         joinedRooms.insert(Common::ProtoHelper::getStr(state.rooms(i), &Protos::GUI::State::Room::name));

   foreach (ChatWidget* chatWidget, this->chatRooms)
      if (!joinedRooms.remove(chatWidget->getRoomName()) && chatWidget->getRoomName() != this->newOpenedChatRoom)
         this->removeWidget(chatWidget);

   foreach (QString roomName, joinedRooms)
      this->addChatWindow(roomName, false);

   this->newOpenedChatRoom.clear();

   if (this->downloadsBusyIndicator)
   {
      if (state.stats().cache_status() == Protos::GUI::State::Stats::LOADING_CACHE_IN_PROGRESS)
         this->downloadsBusyIndicator->show();
      else
         this->downloadsBusyIndicator->hide();
   }
}

void MdiArea::coreConnected()
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
         case Protos::GUI::Settings_Window_WIN_CHAT: this->addChatWindow(); break;
         case Protos::GUI::Settings_Window_WIN_DOWNLOAD: this->addDownloadsWindow(); break;
         case Protos::GUI::Settings_Window_WIN_UPLOAD: this->addUploadsWindow(); break;
      }
   }

   this->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(this->chatWidget->parent()));
}

void MdiArea::coreDisconnected(bool forced)
{
   this->taskbar.setStatus(TaskbarButtonStatus::BUTTON_STATUS_NOPROGRESS);

   this->removeUploadsWindow();
   this->removeDownloadsWindow();
   this->removeChatWindow();
   this->removeAllWindows();

   if (this->downloadsBusyIndicator)
      this->downloadsBusyIndicator->hide();
}

void MdiArea::tabMoved(int, int)
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

void MdiArea::subWindowActivated(QMdiSubWindow* mdiWindow)
{
   if (mdiWindow)
      if (MdiWidget* mdiWidget = dynamic_cast<MdiWidget*>(mdiWindow->widget()))
         mdiWidget->activate();
}

/**
  * Remove and delete a sub window from the MDI area.
  */
void MdiArea::removeWidget(QWidget* widget)
{
   Q_ASSERT(widget);

   if (BrowseWidget* browseWindow = dynamic_cast<BrowseWidget*>(widget))
      this->browseWidgets.removeOne(browseWindow);
   else if (SearchWidget* searchWindow = dynamic_cast<SearchWidget*>(widget))
      this->searchWidgets.removeOne(searchWindow);
   else if (ChatWidget* chatWindow = dynamic_cast<ChatWidget*>(widget))
      this->chatRooms.removeOne(chatWindow);

   // Set a another sub window as active. If we don't do that the windows are all minimised (bug?).
   if (widget == this->currentSubWindow()->widget())
   {
      QList<QMdiSubWindow*> subWindows = this->subWindowList();
      if (subWindows.size() > 1)
         for (int i = 0; i < subWindows.size(); i++)
            if (subWindows[i]->widget() == widget)
            {
               if (i <= 0)
                  this->setActiveSubWindow(subWindows[i+1]);
               else
                  this->setActiveSubWindow(subWindows[i-1]);

               break;
            }
   }

   // We ask to remove the 'MdiSubWindow' as well.
   // The associated tab widget added with 'setTabButton' is automatically removed and deleted.
   this->removeSubWindow(static_cast<QWidget*>(widget->parent()));

   delete widget;
}

void MdiArea::leaveRoom(QWidget* widget)
{
   ChatWidget* room = static_cast<ChatWidget*>(widget);
   this->coreConnection->leaveRoom(room->getRoomName());
   this->removeWidget(widget);
}

void MdiArea::onGlobalProgressChanged(quint64 completed, quint64 total)
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

QString MdiArea::getBusyIndicatorToolTip() const
{
   return tr("Waiting the cache loading process is finished before loading the download queue");
}

void MdiArea::addSettingsWindow()
{
   this->settingsWidget = new SettingsWidget(this->coreConnection, this->sharedDirsModel);
   connect(this->settingsWidget, SIGNAL(languageChanged(QString)), this, SIGNAL(languageChanged(QString)));
   connect(this->settingsWidget, SIGNAL(styleChanged(QString)), this, SIGNAL(styleChanged(QString)));
   this->addSubWindow(this->settingsWidget, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_SETTINGS);
   this->settingsWidget->setWindowState(Qt::WindowMaximized);
}

void MdiArea::removeSettingsWindow()
{
   if (this->settingsWidget)
   {
      this->removeWidget(this->settingsWidget);
      this->settingsWidget = 0;
   }
}

void MdiArea::addChatWindow()
{
   if (this->chatWidget)
      return;

   this->chatWidget = new ChatWidget(this->coreConnection, this->emoticon, this->peerListModel);
   connect(this->chatWidget, SIGNAL(browsePeer(Common::Hash)), this, SLOT(openBrowseWindow(Common::Hash)));
   this->addSubWindow(this->chatWidget, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_CHAT);
   this->chatWidget->setWindowState(Qt::WindowMaximized);
}

void MdiArea::removeChatWindow()
{
   if (this->chatWidget)
   {
      this->removeWidget(this->chatWidget);
      this->chatWidget = 0;
   }
}

void MdiArea::addDownloadsWindow()
{
   if (this->downloadsWidget)
      return;

   this->downloadsWidget = new DownloadsWidget(this->coreConnection, this->peerListModel, this->sharedDirsModel);
   this->addSubWindow(this->downloadsWidget, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_DOWNLOAD);
   this->downloadsWidget->setWindowState(Qt::WindowMaximized);

   connect(this->downloadsWidget, SIGNAL(globalProgressChanged(quint64, quint64)), this, SLOT(onGlobalProgressChanged(quint64, quint64)));

   this->downloadsBusyIndicator = new BusyIndicator();
   this->downloadsBusyIndicator->setObjectName("tabWidget");
   this->downloadsBusyIndicator->setToolTip(this->getBusyIndicatorToolTip());
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, this->downloadsBusyIndicator);
}

void MdiArea::removeDownloadsWindow()
{
   if (this->downloadsWidget)
   {
      this->removeWidget(this->downloadsWidget);
      this->downloadsWidget = 0;
      this->downloadsBusyIndicator = 0;
   }
}

void MdiArea::addUploadsWindow()
{
   if (this->uploadsWidget)
      return;

   this->uploadsWidget = new UploadsWidget(this->coreConnection, this->peerListModel);
   this->addSubWindow(this->uploadsWidget, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_UPLOAD);
   this->uploadsWidget->setWindowState(Qt::WindowMaximized);
}

void MdiArea::removeUploadsWindow()
{
   if (this->uploadsWidget)
   {
      this->removeWidget(this->uploadsWidget);
      this->uploadsWidget = 0;
   }
}

BrowseWidget* MdiArea::addBrowseWindow(const Common::Hash& peerID)
{
   // If there is already a browse for the given peer we show it.
   for (QListIterator<BrowseWidget*> i(this->browseWidgets); i.hasNext();)
   {
      BrowseWidget* widget = i.next();
      if (widget->getPeerID() == peerID)
      {
         widget->refresh();
         this->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(widget->parent()));
         return widget;
      }
   }

   BrowseWidget* browseWindow = new BrowseWidget(this->coreConnection, this->peerListModel, this->sharedDirsModel, peerID);
   this->addSubWindow(browseWindow, Qt::CustomizeWindowHint);
   browseWindow->setWindowState(Qt::WindowMaximized);
   this->browseWidgets << browseWindow;

   QWidget* buttons = new QWidget();
   buttons->setObjectName("tabWidget");

   TabCloseButton* closeButton = new TabCloseButton(browseWindow, buttons);
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));

   TabRefreshButton* refreshButton = new TabRefreshButton(buttons);
   connect(refreshButton, SIGNAL(clicked()), browseWindow, SLOT(refresh()));

   QHBoxLayout* layButtons = new QHBoxLayout(buttons);
   layButtons->setContentsMargins(0, 0, 0, 0);
   layButtons->addWidget(refreshButton);
   layButtons->addWidget(closeButton);

   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, buttons);

   return browseWindow;
}

BrowseWidget* MdiArea::addBrowseWindow(const Common::Hash& peerID, const Protos::Common::Entry& remoteEntry)
{
   BrowseWidget* browseWindow = this->addBrowseWindow(peerID);
   browseWindow->browseTo(remoteEntry);
   return browseWindow;
}

SearchWidget* MdiArea::addSearchWindow(const QString& term, bool searchInOwnFiles)
{
   SearchWidget* searchWindow = new SearchWidget(this->coreConnection, this->peerListModel, this->sharedDirsModel, term, searchInOwnFiles);
   this->addSubWindow(searchWindow, Qt::CustomizeWindowHint);
   searchWindow->setWindowState(Qt::WindowMaximized);
   this->searchWidgets << searchWindow;
   connect(searchWindow, SIGNAL(browse(const Common::Hash&, const Protos::Common::Entry&)), this, SLOT(addBrowseWindow(const Common::Hash&, const Protos::Common::Entry&)));

   TabCloseButton* closeButton = new TabCloseButton(searchWindow);
   closeButton->setObjectName("tabWidget");
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, closeButton);

   return searchWindow;
}

ChatWidget* MdiArea::addChatWindow(const QString& roomName, bool switchTo)
{
   // If the chat room is already open.
   for (QListIterator<ChatWidget*> i(this->chatRooms); i.hasNext();)
   {
      ChatWidget* chatWidget = i.next();
      if (chatWidget->getRoomName() == roomName)
      {
         if (switchTo)
            this->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(chatWidget->parent()));
         return chatWidget;
      }
   }

   QMdiSubWindow* currentWindow = this->currentSubWindow();

   ChatWidget* chatWindow = new ChatWidget(this->coreConnection, this->emoticon, this->peerListModel, roomName);
   connect(chatWindow, SIGNAL(browsePeer(Common::Hash)), this, SLOT(openBrowseWindow(Common::Hash)));
   this->addSubWindow(chatWindow, Qt::CustomizeWindowHint);
   chatWindow->setWindowState(Qt::WindowMaximized);
   this->chatRooms << chatWindow;

   TabCloseButton* closeButton = new TabCloseButton(chatWindow, nullptr, false);
   closeButton->setObjectName("tabWidget");
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(leaveRoom(QWidget*)));
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, closeButton);

   if (!switchTo && currentWindow)
      this->setActiveSubWindow(currentWindow);

   return chatWindow;
}

void MdiArea::removeAllWindows()
{
   foreach (BrowseWidget* widget, this->browseWidgets)
      this->removeWidget(widget);

   foreach (SearchWidget* widget, this->searchWidgets)
      this->removeWidget(widget);

   foreach (ChatWidget* widget, this->chatRooms)
      this->removeWidget(widget);
}
