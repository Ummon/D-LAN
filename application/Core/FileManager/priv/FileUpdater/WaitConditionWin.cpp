#include <QtCore/QDebug>

#if defined(Q_OS_WIN32)
#include <priv/FileUpdater/WaitConditionWin.h>
using namespace FM;

WaitConditionWin::WaitConditionWin()
   : handle(CreateEvent(NULL, FALSE, FALSE, NULL))
{
}

WaitConditionWin::~WaitConditionWin()
{
   CloseHandle(this->handle);
}

void WaitConditionWin::release()
{
   SetEvent(this->handle);
}

bool WaitConditionWin::wait(int timeout)
{
   return WaitForSingleObject(this->handle, timeout == -1 ? INFINITE : timeout) == WAIT_TIMEOUT;
}

void* WaitConditionWin::getHandle()
{
   return this->handle;
}

#endif
