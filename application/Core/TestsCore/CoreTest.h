#ifndef CORETEST_H
#define CORETEST_H

#include <Core.h>
using namespace CoreSpace;

class CoreTest : public Core
{
public:
   CoreTest(const QString& settingsFileName);
};

#endif
