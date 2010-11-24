#include <Log.h>
using namespace CoreSpace;

QSharedPointer<LM::ILogger> Log::logger(LM::Builder::newLogger("Core"));

