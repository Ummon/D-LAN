#include <CoreSupervisor.h>
using namespace StressTests;

#include <algorithm>
#include <random>
#include <stdexcept>

#include <QDir>
#include <QDirIterator>
#include <QFileInfo>
#include <QJsonDocument>
#include <QJsonObject>
#include <QSet>
#include <QThread>

#include <Common/Constants.h>
#include <Common/KnownExtensions.h>
#include <Common/Path.h>
#include <Common/StringUtils.h>
#include <Common/LogManager/Builder.h>
#include <Common/RemoteCoreController/Builder.h>

#include <Paths.h>

#if defined(Q_OS_UNIX)
   #include <unistd.h>
#endif

namespace
{
   const qint64 WRITE_CHUNK_SIZE = 4 * 1024 * 1024; // [byte].
   const int CONNECTION_DELAY = 1000; // [ms]. Delay between the start of the Core and the first connection attempt, and between two attempts.
   const int MAX_NB_CONNECTION_ATTEMPTS = 30;
   const int RESTART_DELAY = 2000; // [ms]. After an unexpected end of the Core.
   const int MAX_BROWSE_DEPTH = 5;
   const QString CORE_OUTPUT_FILENAME("core_output.txt");

   // Search.
   const int SEARCH_DURATION = 7000; // [ms]. The Core forwards the results during 'search_lifetime' (5 s by default).
   const qint64 SEARCH_MIN_CORE_AGE = 10000; // [ms]. The searched Core must be connected since at least this delay.
   const qint64 SEARCH_MIN_FILE_AGE = 10000; // [ms]. The searched file must be unmodified since at least this delay to be indexed.
   const int SEARCH_MIN_WORD_LENGTH = 5;
   const QString UNFINISHED_SUFFIX(".unfinished");
   const QStringList EXTENSION_FILTERS { "avi", "mkv", "mp3", "flac", "jpg", "png", "zip", "iso", "pdf", "txt", "bin", "mp4", "doc" };

   const QString NAME_CHARACTERS("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 _-");
   const QString NAME_SPECIAL_CHARACTERS = QString::fromUtf8("éèàüöçñøåßΩλπ日本語中文한국어");
   const QStringList EXTENSIONS { "", ".bin", ".txt", ".avi", ".mkv", ".mp3", ".flac", ".jpg", ".png", ".zip", ".iso", ".pdf" };

   bool isReservedName(const QString& name)
   {
      // Windows reserved device names, forbidden as filename whatever the extension is.
      static const QStringList RESERVED_NAMES { "CON", "PRN", "AUX", "NUL", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9" };
      const QString baseName = name.section('.', 0, 0).trimmed();
      return RESERVED_NAMES.contains(baseName, Qt::CaseInsensitive);
   }

   QString entryToStr(const Protos::Common::Entry& entry)
   {
      const QString name = QString::fromStdString(entry.name());
      if (name.isEmpty()) // A shared entry.
         return QString::fromStdString(entry.shared_entry().shared_name());
      return QString::fromStdString(entry.path()) + name + (entry.type() == Protos::Common::Entry::DIR ? "/" : "");
   }

   QString sizeToStr(qint64 bytes)
   {
      return QString("%1 MiB").arg(static_cast<double>(bytes) / (1024 * 1024), 0, 'f', 2);
   }
}

CoreSupervisor::CoreSupervisor(int number, bool stoppable, const Config& config, DiskBudget& diskBudget, SearchCoordinator& searchCoordinator, const QString& directory, quint64 seed) :
   number(number),
   stoppable(stoppable),
   config(config),
   diskBudget(diskBudget),
   searchCoordinator(searchCoordinator),
   remoteControlPort(static_cast<quint16>(config.remoteControlBasePort + number)),
   directory(directory),
   roamingDirectory(directory + "/roaming_settings"),
   localDirectory(directory + "/local_settings"),
   sharedDirectory(directory + "/shared_directories"),
   random(seed),
   logger(LM::Builder::newLogger(QString("CoreSupervisor %1").arg(number))),
   writeBuffer((WRITE_CHUNK_SIZE + 3) / 4)
{
   this->coreStopTimeoutTimer.setSingleShot(true);
   connect(&this->coreStopTimeoutTimer, &QTimer::timeout, this, &CoreSupervisor::coreStopTimedOut);

   this->coreRestartTimer.setSingleShot(true);
   connect(&this->coreRestartTimer, &QTimer::timeout, this, &CoreSupervisor::startCore);

   this->reconnectTimer.setSingleShot(true);
   connect(&this->reconnectTimer, &QTimer::timeout, this, &CoreSupervisor::connectToCore);

   this->tickTimer.setSingleShot(true);
   connect(&this->tickTimer, &QTimer::timeout, this, &CoreSupervisor::tick);

   connect(&this->writeTimer, &QTimer::timeout, this, &CoreSupervisor::writeNextChunk);

   this->searchTimer.setSingleShot(true);
   connect(&this->searchTimer, &QTimer::timeout, this, &CoreSupervisor::checkSearch);

   connect(&this->process, &QProcess::finished, this, &CoreSupervisor::coreFinished);

   // The Cores must not receive the Ctrl-C sent to the StressTests console, they are stopped with the 'quit' command.
#if defined(Q_OS_WIN32)
   this->process.setCreateProcessArgumentsModifier([](QProcess::CreateProcessArguments* args) {
      args->flags |= 0x00000200; // CREATE_NEW_PROCESS_GROUP, 'windows.h' isn't included because of its macros.
   });
#elif defined(Q_OS_UNIX)
   this->process.setChildProcessModifier([]() {
      ::setpgid(0, 0);
   });
#endif
}

CoreSupervisor::~CoreSupervisor()
{
   this->abortPendingWrites();
   this->abortSearch();

   // Should not happen: 'stop()' waits the end of the Core.
   if (this->process.state() != QProcess::NotRunning)
   {
      this->process.disconnect(this);
      this->process.kill();
      this->process.waitForFinished(5000);
   }
}

int CoreSupervisor::getNumber() const
{
   return this->number;
}

void CoreSupervisor::start()
{
   QThread::currentThread()->setObjectName(QString("CoreSupervisor %1").arg(this->number));

   if (!this->stoppable)
      this->log("Non-stoppable Core: it's never restarted by the action 'restart_core' (but it's restarted after a crash)");

   if (!this->createDirectories() || !this->writeInitialSettings())
   {
      emit failure(this->number, "Unable to initialize the Core directories");
      return;
   }

   this->connection = RCC::Builder::newCoreConnection();
   this->connection->setAutoStartLocalCore(false);
   connect(this->connection.data(), &RCC::ICoreConnection::connected, this, &CoreSupervisor::coreConnected);
   connect(this->connection.data(), &RCC::ICoreConnection::disconnected, this, &CoreSupervisor::coreDisconnected);
   connect(this->connection.data(), &RCC::ICoreConnection::connectingError, this, &CoreSupervisor::coreConnectingError);
   connect(this->connection.data(), &RCC::ICoreConnection::newState, this, &CoreSupervisor::newState);

   this->startCore();
   this->scheduleNextTick();
}

void CoreSupervisor::stop()
{
   if (this->stopping)
      return;
   this->stopping = true;

   QStringList actions;
   for (auto i = this->nbActions.cbegin(); i != this->nbActions.cend(); ++i)
      actions << QString("%1: %2").arg(actionName(i.key())).arg(i.value());
   this->log(QString("Stopping. Number of actions executed: %1").arg(actions.isEmpty() ? "none" : actions.join(", ")));
   this->log(QString("Searches checked: %1, mismatches: %2").arg(this->nbSearches).arg(this->nbSearchMismatches));

   this->tickTimer.stop();
   this->abortSearch();
   this->reconnectTimer.stop();
   this->coreRestartTimer.stop();
   this->abortPendingWrites();

   auto afterStopped = [this]() {
      this->currentBrowse.clear();
      this->chatMessageResults.clear();
      this->connection.clear();
      emit stopped(this->number);
   };

   if (this->coreStopping) // The Core is being restarted, don't restart it.
      this->afterCoreStopped = afterStopped;
   else
      this->stopCore(afterStopped);
}

/////////////////////////////////////////////////////// Core process ///////////////////////////////////////////////////////

bool CoreSupervisor::createDirectories()
{
   for (const QString& dir : { this->roamingDirectory, this->localDirectory, this->sharedDirectory })
   {
      if (!QDir().mkpath(dir))
      {
         this->logError(QString("Unable to create the directory '%1'").arg(dir));
         return false;
      }
   }
   return true;
}

/**
  * Only the values which differ from the Core default values are written, the Core adds the other ones.
  */
bool CoreSupervisor::writeInitialSettings()
{
   QJsonObject settings;
   settings["nick"] = QString("StressTests %1").arg(this->number);
   settings["unicast_base_port"] = this->config.unicastBasePort + this->config.unicastPortStep * this->number;
   settings["channel"] = this->config.channel;
   settings["multicast_port"] = this->config.multicastPort;

   const QString filepath = this->roamingDirectory + "/" + Common::Constants::CORE_SETTINGS_FILENAME;
   QFile file(filepath);
   if (!file.open(QIODevice::WriteOnly) || file.write(QJsonDocument(settings).toJson()) == -1)
   {
      this->logError(QString("Unable to write the Core settings '%1': %2").arg(filepath, file.errorString()));
      return false;
   }
   return true;
}

void CoreSupervisor::startCore()
{
   if (this->stopping || this->process.state() != QProcess::NotRunning)
      return;

   this->process.setProgram(this->config.coreExecutable);
   // '-r' must not be the first argument: QtService would take it as its 'resume' command.
   this->process.setArguments({ "--port", QString::number(this->remoteControlPort), "-r", this->roamingDirectory, "-l", this->localDirectory });
   this->process.setWorkingDirectory(this->localDirectory);
   // The output must be read or redirected, otherwise the Core may be blocked when the pipe is full.
   // 'stdin' stays a pipe to send the 'quit' command.
   this->process.setProcessChannelMode(QProcess::MergedChannels);
   this->process.setStandardOutputFile(this->localDirectory + "/" + CORE_OUTPUT_FILENAME, QIODevice::Append);

   this->log(QString("Starting the Core: %1 %2").arg(this->process.program(), this->process.arguments().join(' ')));
   this->process.start();
   if (!this->process.waitForStarted(10000))
   {
      this->logError(QString("Unable to start the Core: %1").arg(this->process.errorString()));
      emit failure(this->number, QString("Unable to start the Core '%1': %2").arg(this->config.coreExecutable, this->process.errorString()));
      return;
   }

   this->coreKilled = false;
   this->nbConnectionAttempts = 0;
   this->reconnectTimer.start(CONNECTION_DELAY);
}

/**
  * Send the 'quit' command to the Core and wait its end, then call 'afterStopped'.
  * If the Core doesn't stop in time it's killed, see 'coreStopTimedOut()'.
  */
void CoreSupervisor::stopCore(std::function<void()> afterStopped)
{
   if (this->process.state() == QProcess::NotRunning)
   {
      if (afterStopped)
         afterStopped();
      return;
   }

   this->log("Stopping the Core");

   this->coreStopping = true;
   this->afterCoreStopped = afterStopped;

   this->searchCoordinator.setCoreUnavailable(this->number);
   this->abortSearch();
   this->reconnectTimer.stop();
   this->stateReceived = false;
   this->sharedPathsKnown = false;
   this->currentBrowse.clear();
   this->chatMessageResults.clear();
   if (this->connection)
      this->connection->disconnectFromCore();

   this->process.write("quit\n");
   this->coreStopTimeoutTimer.start(this->config.coreStopTimeoutS * 1000);
}

void CoreSupervisor::coreFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
   this->searchCoordinator.setCoreUnavailable(this->number);
   this->abortSearch();
   this->coreStopTimeoutTimer.stop();
   this->reconnectTimer.stop();
   this->stateReceived = false;
   this->sharedPathsKnown = false;
   this->currentBrowse.clear();
   this->chatMessageResults.clear();
   if (this->connection && (this->connection->isConnected() || this->connection->isConnecting()))
      this->connection->disconnectFromCore();

   const bool crashed = exitStatus == QProcess::CrashExit || exitCode != 0;

   if (this->coreKilled)
   {
      this->log("The Core has been killed");
   }
   else if (crashed)
   {
      const QString description =
         QString("The Core crashed (exit status: %1, exit code: %2 (0x%3)). Logs and crash reports: '%4', output: '%5'")
            .arg(exitStatus == QProcess::CrashExit ? "crash" : "normal")
            .arg(exitCode)
            .arg(QString::number(static_cast<quint32>(exitCode), 16))
            .arg(this->localDirectory + "/log_core")
            .arg(this->localDirectory + "/" + CORE_OUTPUT_FILENAME);
      this->logError(description);
      emit failure(this->number, description);
   }
   else if (!this->coreStopping)
   {
      const QString description = QString("The Core stopped unexpectedly (exit code 0). Output: '%1'").arg(this->localDirectory + "/" + CORE_OUTPUT_FILENAME);
      this->logError(description);
      emit failure(this->number, description);
   }
   else
   {
      this->log("The Core has stopped");
   }

   if (this->coreStopping)
   {
      this->coreStopping = false;
      const auto afterStopped = std::move(this->afterCoreStopped);
      this->afterCoreStopped = nullptr;
      if (afterStopped)
         afterStopped();
   }
   else if (!this->stopping)
   {
      this->log(QString("The Core will be restarted in %1 ms").arg(RESTART_DELAY));
      this->coreRestartTimer.start(RESTART_DELAY);
   }
}

void CoreSupervisor::coreStopTimedOut()
{
   const QString description = QString("The Core didn't stop within %1 s after the 'quit' command, it is killed").arg(this->config.coreStopTimeoutS);
   this->logError(description);
   emit failure(this->number, description);

   this->coreKilled = true;
   this->process.kill();
}

/////////////////////////////////////////////////////// Remote connection ///////////////////////////////////////////////////////

void CoreSupervisor::connectToCore()
{
   if (this->stopping || this->coreStopping || this->process.state() != QProcess::Running || this->connection->isConnected() || this->connection->isConnecting())
      return;

   this->nbConnectionAttempts++;
   this->connection->connectToCore(this->remoteControlPort);
}

void CoreSupervisor::coreConnected()
{
   this->log(QString("Connected to the Core on port %1").arg(this->remoteControlPort));
   this->nbConnectionAttempts = 0;
}

void CoreSupervisor::coreDisconnected(bool asked)
{
   this->abortSearch();
   this->stateReceived = false;
   this->sharedPathsKnown = false;

   if (!asked && !this->stopping && !this->coreStopping && this->process.state() == QProcess::Running)
   {
      this->logWarning("Disconnected from the Core, reconnecting . . .");
      this->reconnectTimer.start(CONNECTION_DELAY);
   }
}

void CoreSupervisor::coreConnectingError(RCC::ICoreConnection::ConnectionErrorCode errorCode)
{
   if (this->stopping || this->coreStopping || this->process.state() != QProcess::Running)
      return;

   if (this->nbConnectionAttempts < MAX_NB_CONNECTION_ATTEMPTS)
   {
      this->reconnectTimer.start(CONNECTION_DELAY);
   }
   else
   {
      this->logError(QString("Unable to connect to the Core on port %1 after %2 attempts (error code: %3), next attempt in %4 s")
         .arg(this->remoteControlPort).arg(this->nbConnectionAttempts).arg(errorCode).arg(10 * CONNECTION_DELAY / 1000));
      this->nbConnectionAttempts = 0;
      this->reconnectTimer.start(10 * CONNECTION_DELAY);
   }
}

void CoreSupervisor::newState(const Protos::GUI::State& state)
{
   const int previousNbPeers = this->stateReceived ? this->state.peers_size() : -1;
   this->state = state;

   if (state.peers_size() > 0)
      this->ownID = Common::Hash(state.peers(0).peer_id().hash());

   if (!this->sharedPathsKnown)
   {
      this->sharedPaths.clear();
      for (const auto& sharedEntry : state.shared_entries())
         this->sharedPaths << QString::fromStdString(sharedEntry.entry().path());
      this->sharedPathsKnown = true;
   }

   if (!this->stateReceived)
   {
      this->stateReceived = true;
      this->log(QString("State received: %1 other peer(s), %2 shared entries, %3 download(s)")
         .arg(std::max(0, state.peers_size() - 1)).arg(state.shared_entries_size()).arg(state.downloads_size()));
   }
   else if (previousNbPeers != state.peers_size())
   {
      this->log(QString("Number of other peers: %1").arg(std::max(0, state.peers_size() - 1)));
   }

   QList<SearchCoordinator::SharedEntry> sharedEntries;
   for (const auto& sharedEntry : state.shared_entries())
      sharedEntries << SearchCoordinator::SharedEntry { Common::Hash(sharedEntry.entry().id().hash()), QString::fromStdString(sharedEntry.entry().path()) };
   this->searchCoordinator.setCoreAvailable(this->number, this->ownID, sharedEntries);

   qint64 pendingDownloadBytes = 0;
   for (const auto& download : state.downloads())
   {
      if (download.status() == Protos::Common::COMPLETE || download.status() == Protos::Common::DELETED)
         continue;
      pendingDownloadBytes += static_cast<qint64>(download.local_entry().size()) - static_cast<qint64>(download.downloaded_bytes());
   }
   this->diskBudget.setPendingDownloadBytes(this->number, std::max<qint64>(0, pendingDownloadBytes));
}

/////////////////////////////////////////////////////// Actions ///////////////////////////////////////////////////////

void CoreSupervisor::scheduleNextTick()
{
   if (this->stopping)
      return;
   this->tickTimer.start(static_cast<int>(this->random.bounded(this->config.tickMinMs, this->config.tickMaxMs + 1)));
}

void CoreSupervisor::tick()
{
   if (this->connection && this->connection->isConnected() && this->stateReceived && this->sharedPathsKnown && !this->coreStopping)
   {
      if (const auto action = this->pickAction())
      {
         this->nbActions[*action]++;
         this->executeAction(*action);
      }
   }
   this->scheduleNextTick();
}

/**
  * A non-stoppable Core never draws 'RESTART_CORE', the other actions keep their relative weights.
  * @return Nothing if no action can be drawn.
  */
std::optional<Action> CoreSupervisor::pickAction()
{
   auto weightOf = [this](Action action, int weight) {
      return action == Action::RESTART_CORE && !this->stoppable ? 0 : weight;
   };

   int total = 0;
   for (auto i = this->config.actionWeights.cbegin(); i != this->config.actionWeights.cend(); ++i)
      total += weightOf(i.key(), i.value());

   if (total == 0)
      return std::nullopt;

   int value = static_cast<int>(this->random.bounded(total));
   for (auto i = this->config.actionWeights.cbegin(); i != this->config.actionWeights.cend(); ++i)
   {
      const int weight = weightOf(i.key(), i.value());
      if (value < weight)
         return i.key();
      value -= weight;
   }
   return std::nullopt; // Not reachable.
}

void CoreSupervisor::executeAction(Action action)
{
   switch (action)
   {
   case Action::CREATE_FILE: this->createFile(); break;
   case Action::CREATE_SHARED_DIRECTORY: this->createSharedDirectory(); break;
   case Action::CREATE_SUB_DIRECTORY: this->createSubDirectory(); break;
   case Action::CHANGE_NICK: this->changeNick(); break;
   case Action::DOWNLOAD: this->download(); break;
   case Action::CANCEL_DOWNLOAD: this->cancelDownload(); break;
   case Action::PAUSE_DOWNLOAD: this->pauseDownload(); break;
   case Action::MOVE_DOWNLOADS: this->moveDownloads(); break;
   case Action::DELETE_ENTRY: this->deleteEntry(); break;
   case Action::JOIN_LEAVE_ROOM: this->joinLeaveRoom(); break;
   case Action::SEND_CHAT_MESSAGE: this->sendChatMessage(); break;
   case Action::RESTART_CORE: this->restartCore(); break;
   case Action::SEARCH: this->search(); break;
   }
}

/**
  * Create a file with random data in 'shared_directories/' or in one of its sub-directories.
  * The file is written chunk by chunk (see 'writeNextChunk()') to keep the event loop responsive.
  * A file created at the root of 'shared_directories/' is shared once written.
  */
void CoreSupervisor::createFile()
{
   const QStringList directories = this->listDirectories(true);
   const QString parent = directories[this->random.bounded(directories.size())];
   const qint64 size = this->randomFileSize();

   if (!this->diskBudget.tryReserve(size))
   {
      this->log(QString("Create file: not enough space left in the budget for %1 (used: %2)").arg(sizeToStr(size), sizeToStr(this->diskBudget.getUsedBytes())));
      return;
   }

   const QString filepath = parent + "/" + this->uniqueName(parent, true);
   auto file = std::make_unique<QFile>(filepath);
   if (!file->open(QIODevice::WriteOnly | QIODevice::NewOnly))
   {
      this->logWarning(QString("Create file: unable to create '%1': %2").arg(filepath, file->errorString()));
      this->diskBudget.release(size);
      return;
   }

   this->log(QString("Create file: '%1' (%2)").arg(filepath, sizeToStr(size)));
   this->pendingWrites.push_back(PendingWrite { std::move(file), size, size, isSamePath(parent, this->sharedDirectory) });
   if (!this->writeTimer.isActive())
      this->writeTimer.start(0);
}

void CoreSupervisor::createSharedDirectory()
{
   const QString path = this->sharedDirectory + "/" + this->uniqueName(this->sharedDirectory, false);
   if (!QDir().mkdir(path))
   {
      this->logWarning(QString("Create shared directory: unable to create '%1'").arg(path));
      return;
   }

   this->log(QString("Create shared directory: '%1'").arg(path));
   this->addSharedPath(path + "/");
}

void CoreSupervisor::createSubDirectory()
{
   const QStringList directories = this->listDirectories(false);
   if (directories.isEmpty())
      return;

   const QString parent = directories[this->random.bounded(directories.size())];
   const QString path = parent + "/" + this->uniqueName(parent, false);
   if (!QDir().mkdir(path))
   {
      this->logWarning(QString("Create sub-directory: unable to create '%1'").arg(path));
      return;
   }

   this->log(QString("Create sub-directory: '%1'").arg(path));
}

void CoreSupervisor::changeNick()
{
   const QString nick = QString("StressTests %1 %2").arg(this->number).arg(this->randomName(false));
   this->log(QString("Change nick: '%1'").arg(nick));
   this->sendCoreSettings(nick);
}

/**
  * Browse a random peer from its roots then go down randomly into its directories to choose a file or a directory to download.
  */
void CoreSupervisor::download()
{
   if (this->currentBrowse)
      return; // A browse is already in progress.

   QList<Common::Hash> peerIDs;
   for (int i = 1; i < this->state.peers_size(); i++)
      if (this->state.peers(i).status() == Protos::GUI::State::Peer::OK)
         peerIDs << Common::Hash(this->state.peers(i).peer_id().hash());

   if (peerIDs.isEmpty())
      return;

   this->browse(peerIDs[this->random.bounded(peerIDs.size())], nullptr, 0);
}

void CoreSupervisor::browse(const Common::Hash& peerID, const Protos::Common::Entry* entry, int depth)
{
   QSharedPointer<RCC::IBrowseResult> result = entry ? this->connection->browse(peerID, *entry) : this->connection->browse(peerID);
   if (result.isNull())
      return;

   this->currentBrowse = result;
   RCC::IBrowseResult* const resultPtr = result.data();

   // The result object must not be deleted while it emits its signal: the result is processed later.
   connect(resultPtr, &RCC::IBrowseResult::result, this, [this, resultPtr, peerID, depth](const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries) {
      const Protos::Common::Entries firstEntries = entries.empty() ? Protos::Common::Entries() : entries.Get(0);
      QMetaObject::invokeMethod(this, [this, resultPtr, peerID, depth, firstEntries]() {
         if (this->currentBrowse.data() != resultPtr)
            return;
         this->currentBrowse.clear();
         this->browseResult(peerID, firstEntries, depth);
      }, Qt::QueuedConnection);
   });

   connect(resultPtr, &Common::Timeoutable::timeout, this, [this, resultPtr]() {
      QMetaObject::invokeMethod(this, [this, resultPtr]() {
         if (this->currentBrowse.data() != resultPtr)
            return;
         this->currentBrowse.clear();
         this->logWarning("Download: browse timed out");
      }, Qt::QueuedConnection);
   });

   result->start();
}

void CoreSupervisor::browseResult(const Common::Hash& peerID, const Protos::Common::Entries& entries, int depth)
{
   if (entries.entries_size() == 0 || this->stopping || !this->stateReceived)
      return;

   const Protos::Common::Entry entry = entries.entries(this->random.bounded(entries.entries_size()));

   // Go deeper or download the entry.
   if (entry.type() == Protos::Common::Entry::DIR && !entry.is_empty() && depth < MAX_BROWSE_DEPTH && this->random.bounded(100) < 60)
      this->browse(peerID, &entry, depth + 1);
   else
      this->downloadEntry(peerID, entry);
}

void CoreSupervisor::downloadEntry(const Common::Hash& peerID, const Protos::Common::Entry& entry)
{
   if (!this->diskBudget.canAfford(static_cast<qint64>(entry.size())))
   {
      this->log(QString("Download: not enough space left in the budget for '%1' (%2)").arg(entryToStr(entry), sizeToStr(entry.size())));
      return;
   }

   // The destination is one of our shared directories, or one of its sub-directories.
   QList<std::pair<Common::Hash, QString>> destinations;
   for (const auto& sharedEntry : this->state.shared_entries())
   {
      const QString path = QString::fromStdString(sharedEntry.entry().path());
      if (path.endsWith('/') && QDir(path).exists())
         destinations << std::make_pair(Common::Hash(sharedEntry.entry().id().hash()), path);
   }

   if (destinations.isEmpty())
   {
      this->log(QString("Download: no shared directory to download '%1'").arg(entryToStr(entry)));
      return;
   }

   const auto& destination = destinations[this->random.bounded(destinations.size())];

   QStringList directories { destination.second };
   QDirIterator i(destination.second, QDir::Dirs | QDir::NoDotAndDotDot | QDir::Hidden, QDirIterator::Subdirectories);
   while (i.hasNext())
      directories << i.next();
   const QString directory = directories[this->random.bounded(directories.size())];
   const QStringList relativeDirs = QDir(destination.second).relativeFilePath(directory).split('/', Qt::SkipEmptyParts);

   try
   {
      const Common::Path relativePath = relativeDirs.isEmpty() || relativeDirs == QStringList { "." } ? Common::Path() : Common::Path(relativeDirs);

      QString peerNick;
      for (const auto& peer : this->state.peers())
         if (Common::Hash(peer.peer_id().hash()) == peerID)
            peerNick = QString::fromStdString(peer.nick());

      this->log(QString("Download: '%1' (%2) from '%3' to '%4'").arg(entryToStr(entry), sizeToStr(entry.size()), peerNick, directory));
      this->connection->download(peerID, entry, destination.first, relativePath);
   }
   catch (const std::invalid_argument& e)
   {
      this->logWarning(QString("Download: invalid destination path '%1': %2").arg(directory, e.what()));
   }
}

void CoreSupervisor::cancelDownload()
{
   if (this->state.downloads_size() == 0)
      return;

   const auto& download = this->state.downloads(this->random.bounded(this->state.downloads_size()));
   const bool complete = this->random.bounded(5) == 0; // Also remove the completed downloads.

   this->log(QString("Cancel download: '%1'%2").arg(entryToStr(download.local_entry()), complete ? " and remove the completed downloads" : ""));
   this->connection->cancelDownloads({ download.id() }, complete);
}

void CoreSupervisor::pauseDownload()
{
   if (this->state.downloads_size() == 0)
      return;

   const auto& download = this->state.downloads(this->random.bounded(this->state.downloads_size()));
   const bool pause = download.status() != Protos::Common::PAUSED;

   this->log(QString("%1 download: '%2'").arg(pause ? "Pause" : "Unpause", entryToStr(download.local_entry())));
   this->connection->pauseDownloads({ download.id() }, pause);
}

void CoreSupervisor::moveDownloads()
{
   if (this->state.downloads_size() < 2)
      return;

   QList<quint64> IDs;
   for (const auto& download : this->state.downloads())
      IDs << download.id();
   std::shuffle(IDs.begin(), IDs.end(), this->random);

   const int nbToMove = 1 + static_cast<int>(this->random.bounded(std::min(3, static_cast<int>(IDs.size()) - 1)));
   const QList<quint64> IDsToMove = IDs.mid(0, nbToMove);
   const quint64 IDRef = IDs[nbToMove];
   const auto position = this->random.bounded(2) == 0 ? Protos::GUI::MoveDownloads::BEFORE : Protos::GUI::MoveDownloads::AFTER;

   this->log(QString("Move %1 download(s) %2 another one").arg(nbToMove).arg(position == Protos::GUI::MoveDownloads::BEFORE ? "before" : "after"));
   this->connection->moveDownloads(IDRef, IDsToMove, position);
}

/**
  * Delete a random file or directory in 'shared_directories/'. If it's a shared entry it's removed from the shared entries.
  */
void CoreSupervisor::deleteEntry()
{
   QStringList entries = this->listEntries();

   // The files being written are excluded.
   for (const auto& pendingWrite : this->pendingWrites)
      entries.removeIf([&](const QString& entry) { return isSamePath(entry, pendingWrite.file->fileName()); });

   if (entries.isEmpty())
      return;

   const QString path = entries[this->random.bounded(entries.size())];
   const QFileInfo info(path);
   const bool isDir = info.isDir();

   if (!this->searchCoordinator.beginDelete(path))
   {
      this->log(QString("Delete %1: '%2' is locked by a search").arg(isDir ? "directory" : "file", path));
      return;
   }
   const bool removed = isDir ? QDir(path).removeRecursively() : QFile::remove(path);
   this->searchCoordinator.endDelete(path);

   if (removed)
      this->log(QString("Delete %1: '%2'").arg(isDir ? "directory" : "file", path));
   else
      this->log(QString("Delete %1: unable to delete '%2' (or a part of it)").arg(isDir ? "directory" : "file", path));

   if (isSamePath(info.absolutePath(), this->sharedDirectory) && !QFileInfo::exists(path))
      this->removeSharedPath(path);
}

void CoreSupervisor::joinLeaveRoom()
{
   const QString room = QString("Room %1").arg(this->random.bounded(this->config.numberOfRooms));

   bool joined = false;
   for (const auto& r : this->state.rooms())
      if (QString::fromStdString(r.name()) == room && r.joined())
         joined = true;

   this->log(QString("%1 room: '%2'").arg(joined ? "Leave" : "Join", room));
   if (joined)
      this->connection->leaveRoom(room);
   else
      this->connection->joinRoom(room);
}

void CoreSupervisor::sendChatMessage()
{
   QStringList joinedRooms;
   for (const auto& room : this->state.rooms())
      if (room.joined())
         joinedRooms << QString::fromStdString(room.name());

   const QString room = !joinedRooms.isEmpty() && this->random.bounded(2) == 0 ? joinedRooms[this->random.bounded(joinedRooms.size())] : QString();
   const QString message = this->randomText(1, 50);

   this->log(QString("Send chat message to %1: '%2'").arg(room.isEmpty() ? "the main chat" : QString("'%1'").arg(room), message));

   QSharedPointer<RCC::ISendChatMessageResult> result = room.isEmpty() ? this->connection->sendChatMessage(message) : this->connection->sendChatMessage(message, room);
   if (result.isNull())
      return;

   this->chatMessageResults << result;
   RCC::ISendChatMessageResult* const resultPtr = result.data();

   // As for the browse results, the result object must not be deleted while it emits its signal.
   auto removeResult = [this, resultPtr]() {
      QMetaObject::invokeMethod(this, [this, resultPtr]() {
         this->chatMessageResults.removeIf([resultPtr](const QSharedPointer<RCC::ISendChatMessageResult>& r) { return r.data() == resultPtr; });
      }, Qt::QueuedConnection);
   };

   connect(resultPtr, &RCC::ISendChatMessageResult::result, this, [this, removeResult](const Protos::GUI::ChatMessageResult& chatMessageResult) {
      if (chatMessageResult.status() != Protos::GUI::ChatMessageResult::OK)
         this->logWarning(QString("Send chat message: the Core returned the status %1").arg(chatMessageResult.status()));
      removeResult();
   });
   connect(resultPtr, &Common::Timeoutable::timeout, this, [this, removeResult]() {
      this->logWarning("Send chat message: timed out");
      removeResult();
   });

   result->start();
}

void CoreSupervisor::restartCore()
{
   if (!this->searchCoordinator.beginRestart(this->number))
   {
      this->log("Restart the Core: one of its files is locked by a search");
      return;
   }

   this->log("Restart the Core");
   this->stopCore([this]() { this->startCore(); });
}

/////////////////////////////////////////////////////// Search ///////////////////////////////////////////////////////

/**
  * Pick a file of another Core, lock it and search it with a random pattern built to match it or not.
  * The result is checked after 'SEARCH_DURATION', see 'checkSearch()'. Our own files are never searched.
  */
void CoreSupervisor::search()
{
   if (this->currentSearch)
      return;

   const QDateTime now = QDateTime::currentDateTimeUtc();

   // The other Cores, seen by our Core and connected since a while.
   QList<Common::Hash> visiblePeers;
   for (int i = 1; i < this->state.peers_size(); i++)
      if (this->state.peers(i).status() == Protos::GUI::State::Peer::OK)
         visiblePeers << Common::Hash(this->state.peers(i).peer_id().hash());

   QList<std::pair<int, SearchCoordinator::CoreInfo>> cores;
   for (int i = 0; i < this->config.numberOfCores; i++)
   {
      SearchCoordinator::CoreInfo info;
      if (i != this->number && this->searchCoordinator.getCoreInfo(i, info) && info.availableSince.msecsTo(now) >= SEARCH_MIN_CORE_AGE && visiblePeers.contains(info.peerID))
         cores << std::make_pair(i, info);
   }

   if (cores.isEmpty())
      return;

   const auto& [coreNumber, coreInfo] = cores[this->random.bounded(cores.size())];

   // The complete files of this Core which are old enough to be indexed.
   auto isEligible = [&](const QFileInfo& fileInfo) {
      return !fileInfo.fileName().endsWith(UNFINISHED_SUFFIX) && fileInfo.lastModified().toUTC().msecsTo(now) >= SEARCH_MIN_FILE_AGE;
   };

   QList<std::pair<QString, int>> files; // Path, index of the shared entry.
   for (int i = 0; i < coreInfo.sharedEntries.size(); i++)
   {
      const QString& sharedEntryPath = coreInfo.sharedEntries[i].path;
      if (sharedEntryPath.endsWith('/'))
      {
         QDirIterator it(sharedEntryPath, QDir::Files | QDir::Hidden, QDirIterator::Subdirectories);
         while (it.hasNext())
         {
            it.next();
            if (isEligible(it.fileInfo()))
               files << std::make_pair(it.filePath(), i);
         }
      }
      else if (const QFileInfo fileInfo(sharedEntryPath); fileInfo.isFile() && isEligible(fileInfo))
      {
         files << std::make_pair(sharedEntryPath, i);
      }
   }

   if (files.isEmpty())
      return;

   const auto& [filepath, sharedEntryIndex] = files[this->random.bounded(files.size())];
   const SearchCoordinator::SharedEntry& sharedEntry = coreInfo.sharedEntries[sharedEntryIndex];

   auto lock = this->searchCoordinator.lockFile(coreNumber, filepath);
   if (!lock)
      return;

   // The file may have been deleted before being locked.
   const QFileInfo fileInfo(filepath);
   if (!fileInfo.isFile())
      return;

   const QString name = fileInfo.fileName();
   const qint64 size = fileInfo.size();
   const QString extension = Common::KnownExtensions::getExtension(name).toLower();

   // A word or a sub-word of the name, long enough to be almost unique. The word index uses the same splitting.
   QStringList words = Common::StringUtils::splitInWordsAndSubWords(name);
   words.removeIf([&](const QString& word) { return word.size() < SEARCH_MIN_WORD_LENGTH || word == extension; });
   if (words.isEmpty())
      return;

   auto search = std::make_unique<CurrentSearch>();
   search->lock = std::move(lock);
   search->coreNumber = coreNumber;
   search->peerID = coreInfo.peerID;
   search->sharedEntryID = sharedEntry.id;
   search->isSharedEntry = !sharedEntry.path.endsWith('/');
   search->filepath = filepath;
   search->size = size;
   if (!search->isSharedEntry)
   {
      search->relativeDirectory = QDir(sharedEntry.path).relativeFilePath(fileInfo.absolutePath());
      if (search->relativeDirectory == ".")
         search->relativeDirectory.clear();
   }

   Protos::Common::FindPattern& pattern = search->pattern;
   pattern.set_pattern(words[this->random.bounded(words.size())].toStdString());
   pattern.set_category(Protos::Common::FindPattern::FILE); // Only the files for the moment.

   // Minimum size, it may exclude the file. 0 means no minimum.
   if (this->random.bounded(3) == 0)
   {
      if (this->random.bounded(2) == 0)
      {
         if (size > 0)
            pattern.set_min_size(1 + this->random.bounded(size));
      }
      else
      {
         pattern.set_min_size(size + 1 + this->random.bounded(size + 1));
      }
   }

   // Maximum size, it may exclude the file. 0 means no maximum.
   if (this->random.bounded(3) == 0)
   {
      if (this->random.bounded(2) == 0 || size < 2)
         pattern.set_max_size(size + this->random.bounded(size + 1));
      else
         pattern.set_max_size(1 + this->random.bounded(size - 1));
   }

   // Extensions, they may include the extension of the file or not. The Core must ignore the case.
   if (this->random.bounded(3) == 0)
   {
      QStringList filters = EXTENSION_FILTERS;
      filters.removeAll(extension);
      std::shuffle(filters.begin(), filters.end(), this->random);
      filters = filters.mid(0, 1 + this->random.bounded(3));
      if (!extension.isEmpty() && this->random.bounded(2) == 0)
         filters.insert(this->random.bounded(filters.size() + 1), extension);

      for (const QString& filter : filters)
         pattern.add_extension_filters((this->random.bounded(4) == 0 ? filter.toUpper() : filter).toStdString());
   }

   QStringList extensionFilters;
   for (const auto& filter : pattern.extension_filters())
      extensionFilters << QString::fromStdString(filter).toLower();

   search->expectedMatch =
      (pattern.min_size() == 0 || size >= static_cast<qint64>(pattern.min_size())) &&
      (pattern.max_size() == 0 || size <= static_cast<qint64>(pattern.max_size())) &&
      (extensionFilters.isEmpty() || !extension.isEmpty() && extensionFilters.contains(extension));

   search->result = this->connection->search(pattern, false);
   if (search->result.isNull())
      return;

   RCC::ISearchResult* const resultPtr = search->result.data();
   connect(resultPtr, &RCC::ISearchResult::result, this, [this, resultPtr](const Protos::Common::FindResult& findResult) {
      if (this->currentSearch && this->currentSearch->result.data() == resultPtr)
         this->currentSearch->results << findResult;
   });

   this->log(QString("Search: %1, file: '%2' (%3 bytes) of Core %4, expected: %5")
      .arg(this->patternToStr(pattern), filepath).arg(size).arg(coreNumber).arg(search->expectedMatch ? "found" : "not found"));

   this->currentSearch = std::move(search);
   this->currentSearch->result->start();
   this->searchTimer.start(SEARCH_DURATION);
}

/**
  * Check the received results:
  *  - Inclusion: the searched file must be found if and only if it matches the pattern.
  *  - Validity: each result must match the pattern.
  * A mismatch is logged but isn't a failure: a UDP datagram may be lost.
  */
void CoreSupervisor::checkSearch()
{
   if (!this->currentSearch)
      return;

   const std::unique_ptr<CurrentSearch> search = std::move(this->currentSearch);
   const Protos::Common::FindPattern& pattern = search->pattern;
   const QString word = QString::fromStdString(pattern.pattern());
   const QString filename = QFileInfo(search->filepath).fileName();

   QStringList extensionFilters;
   for (const auto& filter : pattern.extension_filters())
      extensionFilters << QString::fromStdString(filter).toLower();

   bool found = false;
   int nbEntries = 0;
   QSet<QByteArray> peers;
   QStringList invalidEntries;

   for (const Protos::Common::FindResult& findResult : search->results)
   {
      const Common::Hash peerID(findResult.peer_id().hash());
      peers.insert(QByteArray::fromStdString(findResult.peer_id().hash()));

      for (const auto& entryLevel : findResult.entries())
      {
         const Protos::Common::Entry& entry = entryLevel.entry();
         nbEntries++;

         // A shared entry has no name, its name is the one of the shared entry.
         const QString entryName = QString::fromStdString(entry.name().empty() ? entry.shared_entry().shared_name() : entry.name());
         QString entryDirectory = QString::fromStdString(entry.path());
         while (entryDirectory.startsWith('/'))
            entryDirectory.remove(0, 1);
         while (entryDirectory.endsWith('/'))
            entryDirectory.chop(1);

         if (peerID == search->peerID && Common::Hash(entry.shared_entry().id().hash()) == search->sharedEntryID)
         {
            if (search->isSharedEntry ? entry.name().empty() : entryDirectory == search->relativeDirectory && entryName == filename)
               found = true;
         }

         QStringList problems;
         if (entry.type() != Protos::Common::Entry::FILE)
            problems << "not a file";
         if (pattern.min_size() != 0 && entry.size() < pattern.min_size())
            problems << "size lesser than the minimum";
         if (pattern.max_size() != 0 && entry.size() > pattern.max_size())
            problems << "size greater than the maximum";
         if (!extensionFilters.isEmpty() && !extensionFilters.contains(Common::KnownExtensions::getExtension(entryName).toLower()))
            problems << "extension not in the filters";
         if (!entryName.isEmpty())
         {
            const QStringList entryWords = Common::StringUtils::splitInWordsAndSubWords(entryName);
            if (std::none_of(entryWords.cbegin(), entryWords.cend(), [&](const QString& w) { return w.startsWith(word); }))
               problems << "the name doesn't contain the searched word";
         }

         if (!problems.isEmpty())
            invalidEntries << QString("'%1%2' (%3 bytes): %4")
               .arg(entryDirectory.isEmpty() ? QString() : entryDirectory + "/", entryName).arg(entry.size()).arg(problems.join(", "));
      }
   }

   const bool ok = found == search->expectedMatch && invalidEntries.isEmpty();
   this->nbSearches++;

   const QString summary = QString("%1, file: '%2' of Core %3, expected: %4, found: %5, %6 result(s) from %7 peer(s)")
      .arg(this->patternToStr(pattern), search->filepath).arg(search->coreNumber)
      .arg(search->expectedMatch ? "yes" : "no", found ? "yes" : "no").arg(nbEntries).arg(peers.size());

   if (ok)
   {
      this->log(QString("Search OK: %1").arg(summary));
   }
   else
   {
      this->nbSearchMismatches++;
      QString details;
      if (found != search->expectedMatch)
         details += search->expectedMatch ? " The file wasn't found (a UDP datagram may have been lost)." : " The file was found but it doesn't match the pattern.";
      if (!invalidEntries.isEmpty())
         details += QString(" %1 invalid result(s): %2").arg(invalidEntries.size()).arg(invalidEntries.mid(0, 10).join("; "));
      this->logWarning(QString("Search mismatch: %1.%2").arg(summary, details));
   }

   emit searchChecked(this->number, ok);
}

/**
  * The search result and the lock are released.
  */
void CoreSupervisor::abortSearch()
{
   this->searchTimer.stop();
   if (this->currentSearch)
   {
      this->log(QString("Search aborted: %1").arg(this->patternToStr(this->currentSearch->pattern)));
      this->currentSearch.reset();
   }
}

QString CoreSupervisor::patternToStr(const Protos::Common::FindPattern& pattern) const
{
   QStringList extensions;
   for (const auto& filter : pattern.extension_filters())
      extensions << QString::fromStdString(filter);

   return QString("pattern '%1', size: [%2, %3], extensions: [%4], category: %5")
      .arg(QString::fromStdString(pattern.pattern()))
      .arg(pattern.min_size() == 0 ? QString("-") : QString::number(pattern.min_size()))
      .arg(pattern.max_size() == 0 ? QString("-") : QString::number(pattern.max_size()))
      .arg(extensions.join(", "))
      .arg(QString::fromStdString(Protos::Common::FindPattern::Category_Name(pattern.category())));
}

/////////////////////////////////////////////////////// File writing ///////////////////////////////////////////////////////

/**
  * Write one chunk of the first pending file then put it at the end of the queue.
  */
void CoreSupervisor::writeNextChunk()
{
   if (this->pendingWrites.empty())
   {
      this->writeTimer.stop();
      return;
   }

   PendingWrite& pendingWrite = this->pendingWrites.front();

   const qint64 chunkSize = std::min(pendingWrite.remaining, WRITE_CHUNK_SIZE);
   if (chunkSize > 0)
   {
      this->random.fillRange(this->writeBuffer.data(), (chunkSize + 3) / 4);
      if (pendingWrite.file->write(reinterpret_cast<const char*>(this->writeBuffer.data()), chunkSize) != chunkSize)
      {
         this->logWarning(QString("Unable to write the file '%1': %2").arg(pendingWrite.file->fileName(), pendingWrite.file->errorString()));
         pendingWrite.file->close();
         pendingWrite.file->remove();
         this->diskBudget.release(pendingWrite.size);
         this->pendingWrites.pop_front();
         return;
      }
      pendingWrite.remaining -= chunkSize;
   }

   if (pendingWrite.remaining == 0)
   {
      pendingWrite.file->close();
      this->diskBudget.commit(pendingWrite.size);
      this->log(QString("File written: '%1' (%2)").arg(pendingWrite.file->fileName(), sizeToStr(pendingWrite.size)));
      if (pendingWrite.shareWhenDone)
         this->addSharedPath(pendingWrite.file->fileName());
      this->pendingWrites.pop_front();
   }
   else
   {
      this->pendingWrites.splice(this->pendingWrites.end(), this->pendingWrites, this->pendingWrites.begin());
   }
}

/**
  * The partial files are kept.
  */
void CoreSupervisor::abortPendingWrites()
{
   this->writeTimer.stop();
   for (auto& pendingWrite : this->pendingWrites)
   {
      pendingWrite.file->close();
      this->diskBudget.commit(pendingWrite.size - pendingWrite.remaining);
      this->diskBudget.release(pendingWrite.remaining);
   }
   this->pendingWrites.clear();
}

/////////////////////////////////////////////////////// Shared entries ///////////////////////////////////////////////////////

/**
  * Send our shared paths to the Core, the paths which don't exist anymore are removed.
  * The whole list must be sent each time.
  */
void CoreSupervisor::sendCoreSettings(const QString& newNick)
{
   if (!this->connection || !this->connection->isConnected() || !this->sharedPathsKnown)
      return;

   this->sharedPaths.removeIf([](const QString& path) { return !QFileInfo::exists(path); });

   Protos::GUI::CoreSettings settings;
   if (!newNick.isEmpty())
      settings.set_nick(newNick.toStdString());

   for (const QString& path : this->sharedPaths)
      settings.add_shared_paths()->set_path(path.toStdString());

   // Unchanged, otherwise the Core would rebind its sockets.
   settings.set_listen_address("");
   settings.set_listen_any(this->state.listen_any());

   this->connection->setCoreSettings(settings);
}

/**
  * @param path A directory must end with a '/'.
  */
void CoreSupervisor::addSharedPath(const QString& path)
{
   if (!this->connection || !this->connection->isConnected() || !this->sharedPathsKnown)
   {
      this->logWarning(QString("Unable to share '%1': not connected to the Core").arg(path));
      return;
   }

   this->sharedPaths << path;
   this->sendCoreSettings();
}

void CoreSupervisor::removeSharedPath(const QString& path)
{
   const auto nbRemoved = this->sharedPaths.removeIf([&](const QString& sharedPath) { return isSamePath(sharedPath, path); });
   if (nbRemoved > 0)
      this->sendCoreSettings();
}

/////////////////////////////////////////////////////// Helpers ///////////////////////////////////////////////////////

/**
  * Mostly ASCII characters with some special ones.
  */
QString CoreSupervisor::randomName(bool withExtension)
{
   QString name;
   const int length = 1 + static_cast<int>(this->random.bounded(24));
   for (int i = 0; i < length; i++)
   {
      if (this->random.bounded(10) == 0)
         name += NAME_SPECIAL_CHARACTERS[this->random.bounded(NAME_SPECIAL_CHARACTERS.size())];
      else
         name += NAME_CHARACTERS[this->random.bounded(NAME_CHARACTERS.size())];
   }

   // Windows doesn't allow a trailing space or dot.
   name = name.trimmed();
   if (name.isEmpty() || isReservedName(name))
      name.prepend('x');

   if (withExtension)
      name += EXTENSIONS[this->random.bounded(EXTENSIONS.size())];

   return name;
}

QString CoreSupervisor::uniqueName(const QString& parentDirectory, bool withExtension)
{
   QString name;
   do
      name = this->randomName(withExtension);
   while (QFileInfo::exists(parentDirectory + "/" + name));
   return name;
}

QString CoreSupervisor::randomText(int minNbWords, int maxNbWords)
{
   static const QString LETTERS("abcdefghijklmnopqrstuvwxyz");
   static const QStringList SPECIAL_WORDS { ":)", ":(", ";)", ":D", "http://www.d-lan.net", "<b>bold</b>", "&amp;", QString::fromUtf8("naïve"), QString::fromUtf8("日本語"), QString::fromUtf8("🙂") };

   QStringList words;
   const int nbWords = minNbWords + static_cast<int>(this->random.bounded(maxNbWords - minNbWords + 1));
   for (int i = 0; i < nbWords; i++)
   {
      if (this->random.bounded(15) == 0)
      {
         words << SPECIAL_WORDS[this->random.bounded(SPECIAL_WORDS.size())];
      }
      else
      {
         QString word;
         const int length = 1 + static_cast<int>(this->random.bounded(10));
         for (int j = 0; j < length; j++)
            word += LETTERS[this->random.bounded(LETTERS.size())];
         words << word;
      }
   }
   return words.join(' ');
}

/**
  * Normal distribution clamped to [0, max].
  */
qint64 CoreSupervisor::randomFileSize()
{
   std::normal_distribution<double> distribution(this->config.fileSizeMeanMB, this->config.fileSizeStdDevMB);
   const double sizeMB = std::clamp(distribution(this->random), 0.0, this->config.maxFileSizeMB);
   return static_cast<qint64>(sizeMB * 1024 * 1024);
}

/**
  * All the directories in 'shared_directories/', recursively.
  */
QStringList CoreSupervisor::listDirectories(bool includeSharedRoot) const
{
   QStringList directories;
   if (includeSharedRoot)
      directories << this->sharedDirectory;

   QDirIterator i(this->sharedDirectory, QDir::Dirs | QDir::NoDotAndDotDot | QDir::Hidden, QDirIterator::Subdirectories);
   while (i.hasNext())
      directories << i.next();
   return directories;
}

/**
  * All the files and directories in 'shared_directories/', recursively.
  */
QStringList CoreSupervisor::listEntries() const
{
   QStringList entries;
   QDirIterator i(this->sharedDirectory, QDir::Dirs | QDir::Files | QDir::NoDotAndDotDot | QDir::Hidden | QDir::System, QDirIterator::Subdirectories);
   while (i.hasNext())
      entries << i.next();
   return entries;
}

void CoreSupervisor::log(const QString& message) const
{
   this->logger->log(message, LM::SV_END_USER);
}

void CoreSupervisor::logWarning(const QString& message) const
{
   this->logger->log(message, LM::SV_WARNING);
}

void CoreSupervisor::logError(const QString& message) const
{
   this->logger->log(message, LM::SV_ERROR);
}
