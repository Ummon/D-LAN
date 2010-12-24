#ifndef GUI_IBROWSERESULT_H
#define GUI_IBROWSERESULT_H

#include <google/protobuf/repeated_field.h>

#include <Protos/common.pb.h>

#include <Common/Timeoutable.h>

namespace GUI
{
   class IBrowseResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      IBrowseResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~IBrowseResult() {}
      virtual void start() = 0;

   signals:
      void result(const google::protobuf::RepeatedPtrField<Protos::Common::Entries>&);
   };
}

#endif
