#include <MdiArea.h>
using namespace GUI;

#include <QMdiSubWindow>

#include <Common/Settings.h>

#include <WidgetDocument.h>
#include <TabButtons.h>

MdiArea::MdiArea(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, Taskbar& taskbar, QWidget* parent) :
   QMdiArea(parent),
   coreConnection(coreConnection),
   peerListModel(peerListModel),
   taskbar(taskbar),
   widgetSettings(0),
   widgetChat(0),
   widgetDownloads(0),
   widgetUploads(0),
   downloadsBusyIndicator(0)
{
   this->setObjectName(QString::fromUtf8("mdiArea"));
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

   this->addWidgetSettings();

   connect(this->coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreDisconnected(false); // Initial state.
}

MdiArea::~MdiArea()
{
   this->removeWidgetSettings();
}

void MdiArea::focusNthWindow(int num)
{
   if (num < this->subWindowList().size())
      this->setActiveSubWindow(this->subWindowList()[num]);
}

void MdiArea::closeCurrentWindow()
{
   if (this->currentSubWindow())
   {
      QWidget* widget = this->currentSubWindow()->widget();

      if (dynamic_cast<WidgetBrowse*>(widget) || dynamic_cast<WidgetSearch*>(widget) || (dynamic_cast<WidgetChat*>(widget) && !dynamic_cast<WidgetChat*>(widget)->isGeneral()))
         this->removeWidget(widget);
   }
}

void MdiArea::openWindowBrowse(const Hash& peerID)
{
   this->addWidgetBrowse(peerID);
}

void MdiArea::openWindowSearch(const QString& terms, bool ownFiles)
{
   this->addWidgetSearch(terms, ownFiles);
}

void MdiArea::openWindowChat(const QString& roomName)
{
   this->addWidgetChatRoom(roomName);
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
   {
      return true;
   }

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
         case Protos::GUI::Settings_Window_WIN_CHAT: this->addWidgetChat(); break;
         case Protos::GUI::Settings_Window_WIN_DOWNLOAD: this->addWidgetDownloads(); break;
         case Protos::GUI::Settings_Window_WIN_UPLOAD: this->addWidgetUploads(); break;
      }
   }

   this->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
}

void MdiArea::coreDisconnected(bool forced)
{
   this->taskbar.setStatus(TaskbarButtonStatus::BUTTON_STATUS_NOPROGRESS);

   this->removeWidgetUploads();
   this->removeWidgetDownloads();
   this->removeWidgetChat();
   this->removeAllWidgets();

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

void MdiArea::subWindowActivated(QMdiSubWindow* window)
{
   if (!window)
      return;
   if (WidgetDocument* document = dynamic_cast<WidgetDocument*>(window->widget()))
      document->activate();
}

/**
  * The widget can be a WidgetBrowse, WidgetSearch or WidgetChat (room).
  */
void MdiArea::removeWidget(QWidget* widget)
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

/**
  * Remove and delete a sub window from the MDI area.
  */
void MdiArea::removeMdiSubWindow(QMdiSubWindow* mdiSubWindow)
{
   if (mdiSubWindow)
   {
      // Set a another sub window as active. If we don't do that the windows are all minimised (bug?).
      if (mdiSubWindow == this->currentSubWindow())
      {
         QList<QMdiSubWindow*> subWindows = this->subWindowList();
         if (subWindows.size() > 1)
         {
            int i = subWindows.indexOf(mdiSubWindow);
            if (i <= 0)
               this->setActiveSubWindow(subWindows[i+1]);
            else
               this->setActiveSubWindow(subWindows[i-1]);
         }
      }

      this->removeSubWindow(mdiSubWindow);

      delete mdiSubWindow;
   }
}

void MdiArea::addWidgetSettings()
{
   this->widgetSettings = new WidgetSettings(this->coreConnection, this->sharedDirsModel, this);
   connect(this->widgetSettings, SIGNAL(languageChanged(QString)), this, SIGNAL(languageChanged(QString)));
   connect(this->widgetSettings, SIGNAL(styleChanged(QString)), this, SIGNAL(styleChanged(QString)));
   this->addSubWindow(this->widgetSettings, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_SETTINGS);
   this->widgetSettings->setWindowState(Qt::WindowMaximized);
}

void MdiArea::removeWidgetSettings()
{
   if (this->widgetSettings)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetSettings->parent()));
      this->widgetSettings = 0;
   }
}

void MdiArea::addWidgetChat()
{
   if (this->widgetChat)
      return;

   this->widgetChat = new WidgetChat(this->coreConnection, this->peerListModel, this);
   this->widgetChat->installEventFilterOnInput(this);
   this->addSubWindow(this->widgetChat, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_CHAT);
   this->widgetChat->setWindowState(Qt::WindowMaximized);
}

void MdiArea::removeWidgetChat()
{
   if (this->widgetChat)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
      this->widgetChat = 0;
   }
}

void MdiArea::addWidgetDownloads()
{
   if (this->widgetDownloads)
      return;

   this->widgetDownloads = new WidgetDownloads(this->coreConnection, this->peerListModel, this->sharedDirsModel, this);
   this->addSubWindow(this->widgetDownloads, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_DOWNLOAD);
   this->widgetDownloads->setWindowState(Qt::WindowMaximized);

   connect(this->widgetDownloads, SIGNAL(globalProgressChanged(quint64, quint64)), this, SLOT(onGlobalProgressChanged(quint64, quint64)));

   this->downloadsBusyIndicator = new BusyIndicator();
   this->downloadsBusyIndicator->setObjectName("tabWidget");
   this->downloadsBusyIndicator->setToolTip(this->getBusyIndicatorToolTip());
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, this->downloadsBusyIndicator);
}

void MdiArea::removeWidgetDownloads()
{
   if (this->widgetDownloads)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetDownloads->parent()));
      this->widgetDownloads = 0;
      this->downloadsBusyIndicator = 0;
   }
}

void MdiArea::addWidgetUploads()
{
   if (this->widgetUploads)
      return;

   this->widgetUploads = new WidgetUploads(this->coreConnection, this->peerListModel, this);
   this->addSubWindow(this->widgetUploads, Qt::CustomizeWindowHint);
   this->mdiAreaTabBar->setTabData(this->mdiAreaTabBar->count() - 1, Protos::GUI::Settings_Window_WIN_UPLOAD);
   this->widgetUploads->setWindowState(Qt::WindowMaximized);
}

void MdiArea::removeWidgetUploads()
{
   if (this->widgetUploads)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetUploads->parent()));
      this->widgetUploads = 0;
   }
}

WidgetBrowse* MdiArea::addWidgetBrowse(const Common::Hash& peerID)
{
   // If there is already a browse for the given peer we show it.
   for (QListIterator<WidgetBrowse*> i(this->widgetsBrowse); i.hasNext();)
   {
      WidgetBrowse* widget = i.next();
      if (widget->getPeerID() == peerID)
      {
         widget->refresh();
         this->setActiveSubWindow(static_cast<QMdiSubWindow*>(widget->parent()));
         return widget;
      }
   }

   WidgetBrowse* widgetBrowse = new WidgetBrowse(this->coreConnection, this->peerListModel, this->sharedDirsModel, peerID, this);
   this->addSubWindow(widgetBrowse, Qt::CustomizeWindowHint);
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

WidgetBrowse* MdiArea::addWidgetBrowse(const Common::Hash& peerID, const Protos::Common::Entry& remoteEntry)
{
   WidgetBrowse* widgetBrowse = this->addWidgetBrowse(peerID);
   widgetBrowse->browseTo(remoteEntry);
   return widgetBrowse;
}

WidgetSearch* MdiArea::addWidgetSearch(const QString& term, bool searchInOwnFiles)
{
   WidgetSearch* widgetSearch = new WidgetSearch(this->coreConnection, this->peerListModel, this->sharedDirsModel, term, searchInOwnFiles, this);
   this->addSubWindow(widgetSearch, Qt::CustomizeWindowHint);
   widgetSearch->setWindowState(Qt::WindowMaximized);
   this->widgetsSearch << widgetSearch;
   connect(widgetSearch, SIGNAL(browse(const Common::Hash&, const Protos::Common::Entry&)), this, SLOT(addWidgetBrowse(const Common::Hash&, const Protos::Common::Entry&)));

   TabCloseButton* closeButton = new TabCloseButton(widgetSearch);
   closeButton->setObjectName("tabWidget");
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, closeButton);

   return widgetSearch;
}

WidgetChat* MdiArea::addWidgetChatRoom(const QString& roomName, bool switchTo)
{
   // If the chat room is already open.
   for (QListIterator<WidgetChat*> i(this->widgetsChatRoom); i.hasNext();)
   {
      WidgetChat* chatRoom = i.next();
      if (chatRoom->getRoomName() == roomName)
      {
         if (switchTo)
            this->setActiveSubWindow(static_cast<QMdiSubWindow*>(chatRoom->parent()));
         return chatRoom;
      }
   }

   QMdiSubWindow* currentWindow = this->currentSubWindow();

   WidgetChat* widgetChat = new WidgetChat(this->coreConnection, this->peerListModel, roomName, this);
   widgetChat->installEventFilterOnInput(this);
   this->addSubWindow(widgetChat, Qt::CustomizeWindowHint);
   widgetChat->setWindowState(Qt::WindowMaximized);
   this->widgetsChatRoom << widgetChat;

   TabCloseButton* closeButton = new TabCloseButton(widgetChat, nullptr, false);
   closeButton->setObjectName("tabWidget");
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(leaveRoom(QWidget*)));
   this->mdiAreaTabBar->setTabButton(this->mdiAreaTabBar->count() - 1, QTabBar::RightSide, closeButton);

   if (!switchTo && currentWindow)
      this->setActiveSubWindow(currentWindow);

   return widgetChat;
}

void MdiArea::removeAllWidgets()
{
   foreach (WidgetBrowse* widget, this->widgetsBrowse)
      this->removeWidget(widget);

   foreach (WidgetSearch* widget, this->widgetsSearch)
      this->removeWidget(widget);

   foreach (WidgetChat* widget, this->widgetsChatRoom)
      this->removeWidget(widget);
}
