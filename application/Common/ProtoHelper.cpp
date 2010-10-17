#include <ProtoHelper.h>
using namespace Common;

QString ProtoHelper::getDebugStr(const google::protobuf::Message& mess)
{
   return QString::fromUtf8(mess.DebugString().data());
}
