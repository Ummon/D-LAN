#ifndef GUI_MDIAREA_H
#define GUI_MDIAREA_H

#include <QMdiArea>
#include <QSharedPointer>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <BusyIndicator.h>

#include <Settings/WidgetSettings.h>
#include <Settings/DirListModel.h>
#include <Peers/PeerListModel.h>
#include <Chat/WidgetChat.h>
#include <Downloads/WidgetDownloads.h>
#include <Uploads/WidgetUploads.h>
#include <Browse/WidgetBrowse.h>
#include <Search/WidgetSearch.h>
#include <Taskbar/Taskbar.h>

namespace GUI
{
   class MdiArea : public QMdiArea
   {
      Q_OBJECT
   public:
      explicit MdiArea(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, Taskbar& taskbar, QWidget* parent = 0);
      ~MdiArea();

      void focusNthWindow(int num);
      void closeCurrentWindow();

      void openWindowBrowse(const Common::Hash& peerID);
      void openWindowSearch(const QString& terms, bool ownFiles = false);
      void openWindowChat(const QString& roomName);

   signals:
      void languageChanged(const QString& filename);
      void styleChanged(const QString& path);

   protected:
      void changeEvent(QEvent* event);
      bool eventFilter(QObject* obj, QEvent* event);

   private slots:
      void newState(const Protos::GUI::State& state);
      void coreConnected();
      void coreDisconnected(bool forced);

      void tabMoved(int from, int to);
      void subWindowActivated(QMdiSubWindow* window);

      void removeWidget(QWidget* widget);

      void onGlobalProgressChanged(quint64 completed, quint64 total);

   private:
      QString getBusyIndicatorToolTip() const;

      void removeMdiSubWindow(QMdiSubWindow* mdiSubWindow);

      void addWidgetSettings();
      void removeWidgetSettings();

      void addWidgetChat();
      void removeWidgetChat();

      void addWidgetDownloads();
      void removeWidgetDownloads();

      void addWidgetUploads();
      void removeWidgetUploads();

      WidgetBrowse* addWidgetBrowse(const Common::Hash& peerID);

   private slots:
      WidgetBrowse* addWidgetBrowse(const Common::Hash& peerID, const Protos::Common::Entry& remoteEntry);

   private:
      WidgetSearch* addWidgetSearch(const QString& term, bool searchInOwnFiles = false);

      WidgetChat* addWidgetChatRoom(const QString& roomName, bool switchTo = true);

      void removeAllWidgets();

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      PeerListModel& peerListModel;
      Taskbar& taskbar;

      QTabBar* mdiAreaTabBar;
      WidgetSettings* widgetSettings;
      WidgetChat* widgetChat;
      WidgetDownloads* widgetDownloads;
      WidgetUploads* widgetUploads;
      QList<WidgetBrowse*> widgetsBrowse;
      QList<WidgetSearch*> widgetsSearch;
      QList<WidgetChat*> widgetsChatRoom;

      // This widget is shown on the tab of the downloads page. It is visible only after D-LAN has started and during the loading
      // of the cache (before the downloads are loaded).
      // This widget is owned by the tab bar of the 'QMdiArea'.
      BusyIndicator* downloadsBusyIndicator;

      DirListModel sharedDirsModel;
   };
}

#endif
