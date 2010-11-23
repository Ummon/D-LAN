#include <Log.h>
using namespace Core;

QSharedPointer<LM::ILogger> Log::logger(LM::Builder::newLogger("Core"));

