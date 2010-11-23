#include <priv/Log.h>
using namespace DM;

QSharedPointer<LM::ILogger> Log::logger(LM::Builder::newLogger("DownloadManager"));
