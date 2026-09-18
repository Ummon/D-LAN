#pragma once

#include <memory>

#include <QCache>
#include <QFont>

#include <Chat/EmoticonTextDocument.h>
#include <Emoticons/Emoticons.h>

namespace GUI
{
   // GUI-thread only. Returned documents remain valid until the next get() call.
   class ChatDocumentCache
   {
   public:
      explicit ChatDocumentCache(const Emoticons& emoticons);
      EmoticonTextDocument& get(const QString& markdown, const QFont& font, int width);

   private:
      const Emoticons& emoticons;
      QString emoticonTheme;
      // Bound both the number of documents (128) and their total source length
      // (512K UTF-16 characters). This is a cost budget, not an exact byte count.
      QCache<QString, EmoticonTextDocument> documents { 512 * 1024 };
      std::unique_ptr<EmoticonTextDocument> oversizedDocument;
   };
}
