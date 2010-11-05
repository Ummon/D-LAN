#include <ProtoHelper.h>
using namespace Common;

#include <QRegExp>

QString ProtoHelper::getDebugStr(const google::protobuf::Message& mess)
{
   QString str = QString::fromUtf8(mess.DebugString().data());

   // Very dirty : substitute the bytes representation (ascii + escaped octal number) with a hexadecimal representation.
   // hash: "ID\214\351\t\003\312w\213u\320\236@0o\032\220\016(\033"

   QRegExp rx("hash: \"([^\"]+)\"");

   int pos = 0;
   while ((pos = rx.indexIn(str, pos)) != -1)
   {
      QString hash = rx.cap(1);
      QString hashHex;
      for (int i = 0; i < hash.size(); i++)
      {
         if (hash[i] != '\\')
         {
            hashHex.append(QString::number(hash[i].toAscii(), 16));
         }
         else
         {
            if (hash[i+1] == 'n')
            {
               hashHex.append(QString::number(QChar('\n').toAscii(), 16));
               i += 1;
            }
            else
            {
               bool ok;
               hashHex.append(QString::number(hash.mid(i+1, 3).toInt(&ok, 8), 16));
               i += 3;
            }
         }
      }
      QString newHash = QString("hash: \"%1\"").arg(hashHex);
      str.replace(pos, rx.matchedLength(), newHash);
      pos += newHash.size();
  }

   return str;
}
