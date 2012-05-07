/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <CoreApplication.h>

CoreApplication::CoreApplication(int& argc, char** argv) :
   QCoreApplication(argc, argv)
{
}

#ifdef Q_OS_WIN32
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
#endif
