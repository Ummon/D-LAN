#pragma once

#include <algorithm>

#include <QtGlobal>

namespace RCM
{
   // Progress in hundredths of a percent. The peer's byte count is untrusted.
   inline int uploadProgress(quint64 fileSize, quint64 bytesOwned, int offset)
   {
      if (fileSize == 0)
         return 0;

      const quint64 uploaded = static_cast<quint64>(std::max(offset, 0));
      // Compare before adding so even UINT64_MAX from the peer cannot overflow.
      if (bytesOwned >= fileSize || uploaded >= fileSize - bytesOwned)
         return 10000;

      // Scale in floating point to avoid overflowing a 64-bit integer product.
      // Rounding must not report completion while some bytes are still missing.
      return std::min(9999, static_cast<int>(10000.0L * (bytesOwned + uploaded) / fileSize));
   }
}
