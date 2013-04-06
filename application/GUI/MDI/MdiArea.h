#ifndef GUI_MDIAREA_H
#define GUI_MDIAREA_H

#include <QMdiArea>
#include <QSharedPointer>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <BusyIndicator.h>
#include <Emoticons/Emoticons.h>

#include <Peers/PeerListModel.h>
#include <Settings/SettingsWidget.h>
#include <Settings/DirListModel.h>
#include <Chat/ChatWidget.h>
#include <Downloads/DownloadsWidget.h>
#include <Uploads/UploadsWidget.h>
#include <Browse/BrowseWidget.h>
#include <Search/SearchWidget.h>
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

      void openBrowseWindow(const Common::Hash& peerID);
      void openSearchWindow(const QString& terms, bool ownFiles = false);
      void openChatWindow(const QString& roomName);

   public slots:
      void showDownloads();
      void showUploads();

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
      void subWindowActivated(QMdiSubWindow* mdiWindow);

      void removeWidget(QWidget* widget);

      void leaveRoom(QWidget* widget);

      void onGlobalProgressChanged(quint64 completed, quint64 total);

   private:
      QString getBusyIndicatorToolTip() const;

      void addSettingsWindow();
      void removeSettingsWindow();

      void addChatWindow();
      void removeChatWindow();

      void addDownloadsWindow();
      void removeDownloadsWindow();

      void addUploadsWindow();
      void removeUploadsWindow();

      BrowseWidget* addBrowseWindow(const Common::Hash& peerID);

   private slots:
      BrowseWidget* addBrowseWindow(const Common::Hash& peerID, const Protos::Common::Entry& remoteEntry);

   private:
      SearchWidget* addSearchWindow(const QString& term, bool searchInOwnFiles = false);

      ChatWidget* addChatWindow(const QString& roomName, bool switchTo = true);

      void removeAllWindows();

      Emoticons emoticon;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      PeerListModel& peerListModel;
      Taskbar& taskbar;

      QTabBar* mdiAreaTabBar;

      // Permanent windows.
      SettingsWidget* settingsWidget;
      ChatWidget* chatWidget;
      DownloadsWidget* downloadsWidget;
      UploadsWidget* uploadsWidget;

      QList<BrowseWidget*> browseWidgets;
      QList<SearchWidget*> searchWidgets;
      QList<ChatWidget*> chatRooms;

      // The is to avoid to close a new joined room right after receiving a state without this new room.
      QString newOpenedChatRoom;

      // This widget is shown on the tab of the downloads page. It is visible only after D-LAN has started and during the loading
      // of the cache (before the downloads are loaded).
      // This widget is owned by the tab bar of the 'QMdiArea'.
      BusyIndicator* downloadsBusyIndicator;

      DirListModel sharedDirsModel;
   };
}

#endif
