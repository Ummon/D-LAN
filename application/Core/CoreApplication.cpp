#include <CoreApplication.h>

#include <windows.h>

CoreApplication::CoreApplication(int& argc, char** argv) :
   QCoreApplication(argc, argv)
{
}

bool CoreApplication::winEventFilter(MSG* msg, long* result)
{
   /* Was used to rebind the sockets when the compute wake up.
      The issue is more general because if the network interface is restarted, the multicast socket
      doesn't work anymore...
   if (msg->message == WM_DEVICECHANGE)
   {
      emit resumeFromLowPowerState();
   }

   if (msg->message == WM_POWERBROADCAST)
   {
      if (msg->wParam == PBT_APMRESUMEAUTOMATIC)
      {
         emit resumeFromLowPowerState();
      }
   }*/
   return false;
}
