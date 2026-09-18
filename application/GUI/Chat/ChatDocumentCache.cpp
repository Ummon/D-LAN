#include <Chat/ChatDocumentCache.h>
using namespace GUI;

#include <QTextBlock>
#include <QTextCursor>

ChatDocumentCache::ChatDocumentCache(const Emoticons& emoticons) :
   emoticons(emoticons),
   emoticonTheme(emoticons.getDefaultTheme())
{
}

EmoticonTextDocument& ChatDocumentCache::get(const QString& markdown, const QFont& font, int width)
{
   this->oversizedDocument.reset();
   const QString theme = this->emoticons.getDefaultTheme();
   if (theme != this->emoticonTheme)
   {
      this->documents.clear();
      this->emoticonTheme = theme;
   }

   auto* document = this->documents.object(markdown);
   // Markdown headings and code may have explicit sizes derived from the font
   // at parse time; changing only the default font leaves those formats stale.
   if (document && document->defaultFont() != font)
   {
      this->documents.remove(markdown);
      document = nullptr;
   }
   if (!document)
   {
      auto prepared = std::make_unique<EmoticonTextDocument>(this->emoticons);
      document = prepared.get();
      document->setUndoRedoEnabled(false);
      document->setDefaultFont(font);
      document->setMarkdown(markdown);

      // Apply image alignment once, before sizing, painting or hit-testing.
      QTextCursor cursor(document);
      for (QTextBlock block = document->begin(); block.isValid(); block = block.next())
         for (auto it = block.begin(); !it.atEnd(); ++it)
         {
            const QTextFragment fragment = it.fragment();
            if (fragment.charFormat().isImageFormat())
            {
               QTextCharFormat format;
               format.setVerticalAlignment(QTextCharFormat::AlignMiddle);
               cursor.setPosition(fragment.position());
               cursor.setPosition(fragment.position() + fragment.length(), QTextCursor::KeepAnchor);
               cursor.mergeCharFormat(format);
            }
         }

      // Large messages must still render, but must not evict the entire cache.
      if (markdown.size() > this->documents.maxCost())
         this->oversizedDocument = std::move(prepared);
      else
         this->documents.insert(markdown, prepared.release(), qMax<qsizetype>(4096, markdown.size()));
   }

   // Width changes only require relayout, not another Markdown parse.
   if (document->textWidth() != width)
      document->setTextWidth(width);
   return *document;
}
