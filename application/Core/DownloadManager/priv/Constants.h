#ifndef DOWNLOADMANAGER_CONSTANTS_H
#define DOWNLOADMANAGER_CONSTANTS_H

#include <QString>

namespace DM
{
   const int CHECK_DEAD_PEER_PERIOD = 10000; // [ms]. TODO : create a signal in PeerManager instead of checking continuously.
   const int CHECK_ENTRY_PERIOD = 10000; // [ms]. If the hashes cannot be retrieve, it will recheck periodically.
   const int RESCAN_QUEUE_PERIOD_IF_ERROR = 10000; // [ms]. If one or more download has a status >= 0x20 then all the queue will be periodically rescaned.
   const int FILE_QUEUE_VERSION = 1;
}

#endif
