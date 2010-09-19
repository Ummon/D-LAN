#ifndef PEERMANAGER_CONSTANTS_H
#define PEERMANAGER_CONSTANTS_H

#include <QString>

namespace PM
{
   // If we don't receive any 'IMAlive' message from a peer during PEER_TIMEOUT the peer is considering as dead.
   const int PEER_TIMEOUT = 20; // [s].

   const QString FILE_SETTINGS("settings.txt"); ///< The name of the file cache saved in the home directory.
}

#endif
