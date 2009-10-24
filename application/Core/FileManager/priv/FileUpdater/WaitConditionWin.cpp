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

void WaitConditionWin::wait()
{
   WaitForSingleObject(this->handle, INFINITE);
}

void* WaitConditionWin::getHandle()
{
   return this->handle;
}
