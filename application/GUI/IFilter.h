#ifndef GUI_IFILTER_H
#define GUI_IFILTER_H

namespace GUI
{
   template <typename T>
   class IFilter
   {
   public:
      virtual ~IFilter() {}

      virtual QList<T> getFilteredValues() const = 0;
   };
}

#endif
