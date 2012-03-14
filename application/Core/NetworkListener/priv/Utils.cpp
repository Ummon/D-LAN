#include <priv/Utils.h>
using namespace NL;

#include <QString>
#include <QNetworkInterface>

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

QHostAddress Utils::getCurrentAddressToListenTo()
{
   QString adressToListen = SETTINGS.get<QString>("listenAddress");
   if (adressToListen.isEmpty())
      return SETTINGS.get<quint32>("listenAny") == Protos::Common::Interface::Address::IPv4 ? QHostAddress::Any : QHostAddress::AnyIPv6;

   // Check if the address exists.
   bool hasAnyIPv6 = false;
   for (QListIterator<QHostAddress> i(QNetworkInterface::allAddresses()); i.hasNext();)
   {
      QHostAddress currentAddress = i.next();
      if (currentAddress.toString() == adressToListen)
         return QHostAddress(adressToListen);
      if (currentAddress == QHostAddress::AnyIPv6 || currentAddress == QHostAddress::LocalHostIPv6)
         hasAnyIPv6 = true;
   }

   SETTINGS.set("listenAddress", QString(""));

   if (hasAnyIPv6)
   {
      SETTINGS.set("listenAny", static_cast<quint32>(Protos::Common::Interface::Address::IPv6));
      return QHostAddress::AnyIPv6;
   }
   else
   {
      SETTINGS.set("listenAny", static_cast<quint32>(Protos::Common::Interface::Address::IPv4));
      return QHostAddress::Any;
   }
}

/**
  * Return the multicast group. It can be an IPv6 or an IPv4 group, depending of the current address.
  */
QHostAddress Utils::getMulticastGroup()
{
   const quint32 group = SETTINGS.get<quint32>("multicast_group");
   QHostAddress currentAddressToListentTo = Utils::getCurrentAddressToListenTo();

   if (currentAddressToListentTo.protocol() == QAbstractSocket::IPv4Protocol)
      return QHostAddress(group);
   else // IPv6.
   {
      Q_IPV6ADDR groupIPv6;
      groupIPv6[0] = 0xFF;
      groupIPv6[1] = 0x0e;
      groupIPv6[2] = (group & 0xFF000000) >> 24;
      groupIPv6[3] = (group & 0x00FF0000) >> 16;
      groupIPv6[4] = (group & 0x0000FF00) >> 8;
      groupIPv6[5] = group & 0x000000FF;
      groupIPv6[6] = 0;
      groupIPv6[7] = 0;
      groupIPv6[8] = 0;
      groupIPv6[9] = 0;
      groupIPv6[10] = 0;
      groupIPv6[11] = 0;
      groupIPv6[12] = 0;
      groupIPv6[13] = 0;
      groupIPv6[14] = 0;
      groupIPv6[15] = 0;
      return QHostAddress(groupIPv6);
   }
}
