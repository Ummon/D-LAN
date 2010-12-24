#ifndef GUI_ISEARCHRESULT_H
#define GUI_ISEARCHRESULT_H

#include <Protos/common.pb.h>

#include <Common/Timeoutable.h>

namespace GUI
{
   class ISearchResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      ISearchResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~ISearchResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Common::FindResult&);
   };
}

#endif
