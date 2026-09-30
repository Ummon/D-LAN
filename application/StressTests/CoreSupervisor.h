#pragma once

#include <functional>
#include <list>
#include <memory>
#include <vector>

#include <QObject>
#include <QProcess>
#include <QTimer>
#include <QRandomGenerator>
#include <QSharedPointer>
#include <QFile>
#include <QMap>

#include <Protos/gui_protocol.pb.h>

#include <Common/LogManager/ILogger.h>
#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/RemoteCoreController/IBrowseResult.h>
#include <Common/RemoteCoreController/ISendChatMessageResult.h>

#include <Config.h>
#include <DiskBudget.h>

namespace StressTests
{
   /**
     * Launches one D-LAN Core, keeps a remote connection to it and executes a random action at each tick.
     * A CoreSupervisor must be created in its own thread, all its methods are called from this thread.
     *
     * Directory layout, in 'directory':
     *  - roaming_settings/: the Core settings.
     *  - local_settings/: hash cache, download queue, chat, logs, etc. and the Core output (stdout + stderr).
     *  - shared_directories/: the shared entries (directories and files) are created in this directory.
     */
   class CoreSupervisor : public QObject
   {
      Q_OBJECT
   public:
      CoreSupervisor(int number, const Config& config, DiskBudget& diskBudget, const QString& directory, quint64 seed);
      ~CoreSupervisor() override;

      int getNumber() const;

   public slots:
      void start();
      void stop();

   signals:
      /**
        * Emitted when the Core crashes or can't be stopped properly.
        */
      void failure(int number, const QString& description);

      /**
        * Emitted once the Core has been stopped after a call to 'stop()'.
        */
      void stopped(int number);

   private:
      // Core process.
      bool createDirectories();
      bool writeInitialSettings();
      void startCore();
      void stopCore(std::function<void()> afterStopped);
      void coreFinished(int exitCode, QProcess::ExitStatus exitStatus);
      void coreStopTimedOut();

      // Remote connection.
      void connectToCore();
      void coreConnected();
      void coreDisconnected(bool asked);
      void coreConnectingError(RCC::ICoreConnection::ConnectionErrorCode errorCode);
      void newState(const Protos::GUI::State& state);

      // Actions.
      void scheduleNextTick();
      void tick();
      Action pickAction();
      void executeAction(Action action);

      void createFile();
      void createSharedDirectory();
      void createSubDirectory();
      void changeNick();
      void download();
      void cancelDownload();
      void pauseDownload();
      void moveDownloads();
      void deleteEntry();
      void joinLeaveRoom();
      void sendChatMessage();
      void restartCore();

      void browse(const Common::Hash& peerID, const Protos::Common::Entry* entry, int depth);
      void browseResult(const Common::Hash& peerID, const Protos::Common::Entries& entries, int depth);
      void downloadEntry(const Common::Hash& peerID, const Protos::Common::Entry& entry);

      // File writing.
      struct PendingWrite
      {
         std::unique_ptr<QFile> file;
         qint64 size;
         qint64 remaining;
         bool shareWhenDone; ///< True if the file is put at the root of 'shared_directories/'.
      };
      void writeNextChunk();
      void abortPendingWrites();

      // Shared entries.
      void sendCoreSettings(const QString& newNick = QString());
      void addSharedPath(const QString& path);
      void removeSharedPath(const QString& path);

      // Helpers.
      QString randomName(bool withExtension);
      QString uniqueName(const QString& parentDirectory, bool withExtension);
      QString randomText(int minNbWords, int maxNbWords);
      qint64 randomFileSize();
      QStringList listDirectories(bool includeSharedRoot) const;
      QStringList listEntries() const;
      static bool isSamePath(const QString& path1, const QString& path2);

      void log(const QString& message) const;
      void logWarning(const QString& message) const;
      void logError(const QString& message) const;

      const int number;
      const Config config;
      DiskBudget& diskBudget;
      const quint16 remoteControlPort;

      const QString directory;
      const QString roamingDirectory;
      const QString localDirectory;
      const QString sharedDirectory;

      QRandomGenerator random;
      QSharedPointer<LM::ILogger> logger;

      bool stopping = false; ///< True after 'stop()' has been called.

      QProcess process;
      bool coreStopping = false; ///< True while waiting the end of the Core process after a 'quit' command.
      bool coreKilled = false; ///< True when the Core has been killed because it didn't stop in time.
      std::function<void()> afterCoreStopped;
      QTimer coreStopTimeoutTimer;
      QTimer coreRestartTimer; ///< To restart the Core after an unexpected end.

      QSharedPointer<RCC::ICoreConnection> connection;
      QTimer reconnectTimer;
      int nbConnectionAttempts = 0;

      bool stateReceived = false; ///< Reset each time the connection is lost.
      Protos::GUI::State state;
      Common::Hash ownID;

      // Our shared paths (directories end with '/'), 'sharedPaths' is set from the first state received after each connection.
      bool sharedPathsKnown = false;
      QStringList sharedPaths;

      QTimer tickTimer;
      QMap<Action, int> nbActions;

      QSharedPointer<RCC::IBrowseResult> currentBrowse;
      QList<QSharedPointer<RCC::ISendChatMessageResult>> chatMessageResults;

      std::list<PendingWrite> pendingWrites;
      QTimer writeTimer;
      std::vector<quint32> writeBuffer; ///< Random data, see 'writeNextChunk()'.
   };
}
