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
#include <QThread>

#include <Common/Constants.h>
#include <Common/Path.h>
#include <Common/LogManager/Builder.h>
#include <Common/RemoteCoreController/Builder.h>

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

CoreSupervisor::CoreSupervisor(int number, const Config& config, DiskBudget& diskBudget, const QString& directory, quint64 seed) :
   number(number),
   config(config),
   diskBudget(diskBudget),
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

   this->tickTimer.stop();
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
      const Action action = this->pickAction();
      this->nbActions[action]++;
      this->executeAction(action);
   }
   this->scheduleNextTick();
}

Action CoreSupervisor::pickAction()
{
   int total = 0;
   for (int weight : this->config.actionWeights)
      total += weight;

   int value = static_cast<int>(this->random.bounded(total));
   for (auto i = this->config.actionWeights.cbegin(); i != this->config.actionWeights.cend(); ++i)
   {
      if (value < i.value())
         return i.key();
      value -= i.value();
   }
   return Action::CREATE_FILE; // Not reachable.
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

   const bool removed = isDir ? QDir(path).removeRecursively() : QFile::remove(path);
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
   this->log("Restart the Core");
   this->stopCore([this]() { this->startCore(); });
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

bool CoreSupervisor::isSamePath(const QString& path1, const QString& path2)
{
   auto normalize = [](const QString& path) {
      QString normalized = QDir::cleanPath(QDir::fromNativeSeparators(path));
      while (normalized.size() > 1 && normalized.endsWith('/'))
         normalized.chop(1);
      return normalized;
   };

#if defined(Q_OS_WIN32) || defined(Q_OS_DARWIN)
   return normalize(path1).compare(normalize(path2), Qt::CaseInsensitive) == 0;
#else
   return normalize(path1) == normalize(path2);
#endif
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
