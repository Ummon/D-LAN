#ifndef GUI_DOWNLOAD_FILTER_STATUS_H
#define GUI_DOWNLOAD_FILTER_STATUS_H

namespace GUI
{
   enum DownloadFilterStatus
   {
      STATUS_COMPLETE = 0x01,
      STATUS_DOWNLOADING = 0x02,
      STATUS_QUEUED = 0x04,
      STATUS_ERROR = 0x08,
   };
}

#endif
