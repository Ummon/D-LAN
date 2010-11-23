#include <Log.h>
using namespace GUI;

QSharedPointer<LM::ILogger> Log::logger(LM::Builder::newLogger("GUI"));
