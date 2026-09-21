#pragma once

#include <QNetworkInterface>

namespace Common
{
   // Classify on the core's platform, including when its GUI runs on another OS.
   enum class InterfaceKind { LAN, Auxiliary, Tunnel };

   inline InterfaceKind getInterfaceKind(const QNetworkInterface& interface)
   {
#ifdef Q_OS_MACOS
      const QString name = interface.name();
      if (name.startsWith("awdl") || name.startsWith("llw"))
         return InterfaceKind::Auxiliary;
      if (name.startsWith("utun") || name.startsWith("gif") || name.startsWith("stf") ||
          interface.flags().testFlag(QNetworkInterface::IsPointToPoint))
         return InterfaceKind::Tunnel;
#else
      Q_UNUSED(interface);
#endif
      return InterfaceKind::LAN;
   }

   inline bool isDefaultMulticastInterface(const QNetworkInterface& interface)
   {
      return interface.flags().testFlags(QNetworkInterface::IsUp | QNetworkInterface::IsRunning | QNetworkInterface::CanMulticast) &&
         !interface.flags().testFlag(QNetworkInterface::IsLoopBack) &&
         getInterfaceKind(interface) == InterfaceKind::LAN;
   }
}
