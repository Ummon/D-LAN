#pragma once

#include <QString>
#include <QMap>
#include <QList>

namespace StressTests
{
   enum class Action
   {
      CREATE_FILE,
      CREATE_SHARED_DIRECTORY,
      CREATE_SUB_DIRECTORY,
      CHANGE_NICK,
      DOWNLOAD,
      CANCEL_DOWNLOAD,
      PAUSE_DOWNLOAD,
      MOVE_DOWNLOADS,
      DELETE_ENTRY,
      JOIN_LEAVE_ROOM,
      SEND_CHAT_MESSAGE,
      RESTART_CORE,
      SEARCH
   };

   QString actionName(Action action);
   QList<Action> allActions();

   /**
     * The configuration of a stress run, read from a JSON file.
     * Each missing value takes its default value, see 'StressTests.example.json'.
     */
   struct Config
   {
      int numberOfCores = 10;
      double durationMinutes = 15.0;

      int tickMinMs = 500; ///< Each CoreSupervisor waits a random time in [tickMinMs, tickMaxMs] between two actions.
      int tickMaxMs = 2000;

      // File sizes follow a normal distribution clamped to [0, maxFileSizeMB].
      double fileSizeMeanMB = 100.0;
      double fileSizeStdDevMB = 100.0;
      double maxFileSizeMB = 1024.0;

      double maxTotalSizeGB = 100.0; ///< Maximum size of the whole stress test directory.

      int remoteControlBasePort = 59500; ///< Core 'i' is remotely controlled on port 'remoteControlBasePort + i'.
      int unicastBasePort = 59600; ///< Core 'i' uses 'unicastBasePort + unicastPortStep * i' as unicast base port.
      int unicastPortStep = 10;

      // To isolate the Cores from the real D-LAN peers on the LAN.
      QString channel = "D-LAN_StressTests"; ///< Defines the IPv6 multicast group.
      int multicastPort = 59450;

      int numberOfRooms = 5; ///< Chat rooms are chosen among this number of room names.

      double nonStoppableCoresRatio = 0.5; ///< This part of the Cores, chosen randomly, is never restarted by the action 'restart_core', to test long runs.

      int coreStopTimeoutS = 30; ///< A Core which doesn't stop within this delay after 'quit' is killed and counted as a failure.

      QString coreExecutable; ///< Default: the D-LAN Core next to the StressTests executable.

      quint64 seed = 0; ///< 0 -> random seed.

      QMap<Action, int> actionWeights; ///< From 0 (never) to 10 (highly probable).

      Config();

      /**
        * Read the given JSON file. Values that aren't defined in the file keep their default value.
        * @return An error message, empty if OK.
        */
      QString load(const QString& filepath);

      QString toString() const;
   };
}
