#include <ProtoHelper.h>
using namespace Common;

#include <QRegExp>

QString ProtoHelper::getDebugStr(const google::protobuf::Message& mess)
{
   return QString("");

   std::string debugString = mess.DebugString();
   QString str = QString::fromStdString(debugString);

   //return str;

   QString test;
   test.append(QString("%1").arg(5, 2, 16, QLatin1Char('0')));

   // Very dirty : substitute the bytes representation (ascii + escaped octal number) with a hexadecimal representation.
   // hash: "ID\214\351\t\003\312w\213u\320\236@0o\032\220\"(\033"

   const QString prefix("hash: \"");

   int pos = 0;
   while ((pos = str.indexOf(prefix, pos)) != -1)
   {
      pos += prefix.size();
      const int initialPos = pos;
      QString hashHex;
      hashHex.reserve(40);
      while (str[pos] != '"')
      {
         if (str[pos] != '\\')
         {
            hashHex.append(QString::number(str[pos].toAscii(), 16));
            pos++;
         }
         else
         {
            switch (str[pos+1].toAscii())
            {
            case 'r':
               hashHex.append("0d");
               pos += 2;
               break;
            case 'n':
               hashHex.append("0a");
               pos += 2;
               break;
            case '"':
            case '\'':
            case '\\':
               hashHex.append(QString::number(str[pos+1].toAscii(), 16));
               pos += 2;
               break;
            default: // It's an octal number, for example : "\123"
               bool ok;
               hashHex.append(QString("%1").arg(str.mid(pos+1, 3).toInt(&ok, 8), 2, 16, QLatin1Char('0')));
               pos += 4;
            }
         }
      }
      /* Used during debugging :
      const int length = hashHex.size();
      const QString hash = str.mid(initialPos, pos - initialPos);*/

      str.replace(initialPos, pos - initialPos, hashHex);
  }

   return str;
}
