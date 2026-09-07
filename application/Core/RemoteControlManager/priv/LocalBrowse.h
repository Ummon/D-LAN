#pragma once

#include <QFuture>
#include <QThreadPool>
#include <functional>

#include <Protos/gui_protocol.pb.h>

namespace RCM
{
   // Separate from transfer workers so slow filesystem calls cannot occupy their threads.
   QThreadPool& localBrowsePool();
   struct LocalBrowseJob
   {
      QFuture<Protos::GUI::LocalBrowseResult> future;
      std::function<void()> cancel;
   };
   LocalBrowseJob localBrowse(const Protos::GUI::LocalBrowse& request);
}
