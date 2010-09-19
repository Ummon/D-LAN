#ifndef PEERMANAGER_CONSTANTS_H
#define PEERMANAGER_CONSTANTS_H

#include <QString>

namespace PM
{
   // If we don't receive any 'IMAlive' message from a peer during PEER_TIMEOUT the peer is considering as dead.
   const int PEER_TIMEOUT = 20; // [s].

   // Some idle connection can exist for this duration.
   const int IDLE_SOCKET_TIMEOUT = 10 * 60; // [s].

   // The maximum number of idle socket per distant peer.
   const int MAX_NUMBER_IDLE_SOCKET = 1;

   const QString FILE_SETTINGS("settings.txt"); ///< The name of the file cache saved in the home directory.
}

#endif
