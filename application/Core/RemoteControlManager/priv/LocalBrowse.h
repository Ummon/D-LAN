#pragma once

#include <QFuture>
#include <QThreadPool>

#include <Protos/gui_protocol.pb.h>

namespace RCM
{
   // Separate from transfer workers so slow filesystem calls cannot occupy their threads.
   QThreadPool& localBrowsePool();
   QFuture<Protos::GUI::LocalBrowseResult> localBrowse(const Protos::GUI::LocalBrowse& request);
}
