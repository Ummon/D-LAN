#include <priv/Log.h>
using namespace FM;

QSharedPointer<LM::ILogger> Log::logger(LM::Builder::newLogger("FileManager"));
