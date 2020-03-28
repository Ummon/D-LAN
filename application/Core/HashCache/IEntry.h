#pragma once

#include <QString>

namespace HC
{
   class IEntry
   {
   public:
      virtual ~IEntry() {}

      virtual QString getName() = 0;
   };
}
