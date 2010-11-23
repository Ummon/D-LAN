#include <priv/Log.h>
using namespace NL;

QSharedPointer<LM::ILogger> Log::logger(LM::Builder::newLogger("NetworkListener"));

