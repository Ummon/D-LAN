#include <QtTest>
#include <QListView>
#include <QTemporaryDir>
#include <QAbstractTextDocumentLayout>
#include <QHeaderView>
#include <QPainter>
#include <QPointer>
#include <QStandardItemModel>
#include <QTableView>
#include <QTextBlock>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/RemoteCoreController/priv/CoreConnection.h>
#include <Chat/ChatWidget.h>
#include <Chat/ChatTextEdit.h>

namespace
{
   class PendingResult : public RCC::ISendChatMessageResult
   {
   public:
      PendingResult() : ISendChatMessageResult(60000) {}
      void start() override {}
   };

   class Connection : public RCC::CoreConnection
   {
   public:
      int sentMessages = 0;
      QList<Common::Hash> answers;

      QSharedPointer<RCC::ISendChatMessageResult> sendChatMessage(
         const QString&, const QString&, const QList<Common::Hash>& answers) override
      {
         ++this->sentMessages;
         this->answers = answers;
         return QSharedPointer<PendingResult>::create();
      }
   };

   struct Fixture
   {
      QSharedPointer<Connection> connection = QSharedPointer<Connection>::create();
      GUI::Emoticons emoticons { "nonexistent-test-emoticons" };
      GUI::ChatWidget widget { connection, emoticons };
      GUI::ChatTextEdit* editor = widget.findChild<GUI::ChatTextEdit*>("txtMessage");
      GUI::AutoComplete* completion = widget.findChild<GUI::AutoComplete*>();
      QListView* list = completion->findChild<QListView*>();

      Fixture(const QStringList& peers = { "Alice", "Alex", "Bob" })
      {
         Protos::Common::ChatMessages messages;
         for (const auto& nick : peers)
         {
            auto* message = messages.add_messages();
            message->set_id(messages.messages_size());
            message->set_time(1000);
            message->set_peer_nick(nick.toStdString());
            message->set_message("Hello");
            const auto id = Common::Hash::rand();
            message->mutable_peer_id()->set_hash(id.getData(), Common::Hash::HASH_SIZE);
         }
         emit connection->newChatMessages(messages);
         widget.show();
         editor->setFocus();
      }

      void key(int code, const QString& text = {})
      {
         QWidget* target = completion->isVisible() ? static_cast<QWidget*>(list) : editor;
         QKeyEvent event(QEvent::KeyPress, code, Qt::NoModifier, text);
         QApplication::sendEvent(target, &event);
      }

      void type(const QString& text)
      {
         for (const auto c : text)
            key(c == ' ' ? Qt::Key_Space : 0, QString(c));
      }
   };
}

class TestsChatCompletion : public QObject
{
   Q_OBJECT

private slots:
   void draftHistoryDoesNotRetainFullText()
   {
      Fixture f;
      // Avoid measuring layout of a large draft; contentsChange still updates reply history.
      f.editor->document()->setLayoutEnabled(false);
      QTextCursor cursor = f.editor->textCursor();
      const QString chunk = QString(1023, QChar('x')) + '\n';
      for (int i = 0; i < 200; ++i)
      {
         cursor.beginEditBlock();
         cursor.insertText(chunk);
         cursor.endEditBlock();
      }
      QCOMPARE(f.editor->toPlainText(), chunk.repeated(200));
      QVERIFY(f.widget.answerHistory.size() >= 200);
      qsizetype retainedBytes = 0;
      for (const auto& state : f.widget.answerHistory)
      {
         retainedBytes += state.textFingerprint.size();
         QVERIFY(state.answers.isEmpty());
      }
      // The old full-text snapshots retained 41,164,800 bytes for this 400 KiB draft.
      QVERIFY(retainedBytes < 16 * 1024);

      for (int i = 0; i < 200; ++i)
         f.editor->undo();
      QVERIFY(f.editor->toPlainText().isEmpty());
      for (int i = 0; i < 200; ++i)
         f.editor->redo();
      QCOMPARE(f.editor->toPlainText(), chunk.repeated(200));
      f.editor->clear();
      QCOMPARE(f.widget.answerHistory.size(), 1);
      QVERIFY(f.widget.previousMessageText.isEmpty());
      QVERIFY(f.widget.getPeerAnswers().isEmpty());
   }

   void replyReferencesSurviveUndoRedo()
   {
      const QString nick = QString::fromUcs4(U"Alice\U0001f600");
      Fixture f({ nick });
      f.type("@Ali");
      f.key(Qt::Key_Tab);
      const auto peers = f.widget.getPeerAnswers();
      QCOMPARE(peers.size(), 1);
      const QString completed = f.editor->toPlainText();

      QTextCursor cursor = f.editor->textCursor();
      cursor.setPosition(0);
      cursor.beginEditBlock();
      cursor.insertText("prefix ");
      cursor.endEditBlock();
      QCOMPARE(f.widget.getPeerAnswers(), peers);
      f.editor->undo();
      QCOMPARE(f.editor->toPlainText(), completed);
      QCOMPARE(f.widget.getPeerAnswers(), peers);
      f.editor->redo();
      QCOMPARE(f.widget.getPeerAnswers(), peers);

      // Formatting changes preserve the text and its reply reference.
      cursor.select(QTextCursor::Document);
      QTextCharFormat format;
      format.setFontWeight(QFont::Bold);
      cursor.mergeCharFormat(format);
      QCOMPARE(f.widget.getPeerAnswers(), peers);
      f.editor->undo();
      QCOMPARE(f.widget.getPeerAnswers(), peers);
      f.editor->redo();
      QCOMPARE(f.widget.getPeerAnswers(), peers);

      // Replace one character inside the mention, leaving the draft length unchanged.
      cursor.setPosition(8);
      cursor.setPosition(9, QTextCursor::KeepAnchor);
      cursor.insertText("Z");
      QVERIFY(f.widget.getPeerAnswers().isEmpty());
      f.editor->undo();
      QCOMPARE(f.widget.getPeerAnswers(), peers);
      f.editor->redo();
      QVERIFY(f.widget.getPeerAnswers().isEmpty());
      f.editor->undo();
      QCOMPARE(f.widget.getPeerAnswers(), peers);
      f.editor->clear();
      QVERIFY(f.widget.getPeerAnswers().isEmpty());
      QCOMPARE(f.widget.answerHistory.size(), 1);
   }

   void replacingRedoBranchDoesNotRestoreReplies()
   {
      Fixture f;
      f.type("@Ali");
      f.key(Qt::Key_Tab);
      QCOMPARE(f.widget.getPeerAnswers().size(), 1);
      f.editor->undo();
      QCOMPARE(f.editor->toPlainText(), QString("@Ali"));
      QVERIFY(f.widget.getPeerAnswers().isEmpty());

      // Recreate exactly the discarded completion's text as an ordinary edit.
      QTextCursor cursor = f.editor->textCursor();
      cursor.setPosition(1);
      cursor.movePosition(QTextCursor::End, QTextCursor::KeepAnchor);
      cursor.insertText("Alice ");
      QCOMPARE(f.editor->toPlainText(), QString("@Alice "));
      QVERIFY(!f.editor->document()->isRedoAvailable());
      QVERIFY(f.widget.getPeerAnswers().isEmpty());
      f.editor->undo();
      QVERIFY(f.widget.getPeerAnswers().isEmpty());
      f.editor->redo();
      QCOMPARE(f.editor->toPlainText(), QString("@Alice "));
      QVERIFY(f.widget.getPeerAnswers().isEmpty());
   }

   void chatDocumentsReuseAndRelayout()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      GUI::ChatDocumentCache cache(emoticons);
      QFont font = QApplication::font();
      font.setPointSize(10);
      const QString markdown = "# Heading\n\nA **bold** message with a soft\nwrap, `code` and [a link](https://example.com).\n\n![smile](emoticons://missing/smile)";
      auto& document = cache.get(markdown, font, 400);
      QPointer<QTextDocument> original = &document;
      QSignalSpy changed(&document, &QTextDocument::contentsChanged);
      const QSizeF initialSize = document.size();
      for (int i = 0; i < 10; ++i)
      {
         QCOMPARE(&cache.get(markdown, font, 400), original.data());
         QCOMPARE(document.size(), initialSize);
         document.documentLayout()->anchorAt(QPointF(10, 10));
      }
      QCOMPARE(changed.count(), 0);
      QVERIFY(document.toPlainText().contains("soft wrap"));

      for (const int width : { 120, 500 })
      {
         auto& resized = cache.get(markdown, font, width);
         QCOMPARE(&resized, original.data());
         GUI::ChatDocumentCache fresh(emoticons);
         auto& expected = fresh.get(markdown, font, width);
         QCOMPARE(resized.size(), expected.size());
         QCOMPARE(resized.toPlainText(), expected.toPlainText());
      }
      bool foundImage = false;
      for (auto block = document.begin(); block.isValid(); block = block.next())
         for (auto it = block.begin(); !it.atEnd(); ++it)
            if (it.fragment().charFormat().isImageFormat())
            {
               foundImage = true;
               QCOMPARE(it.fragment().charFormat().verticalAlignment(), QTextCharFormat::AlignMiddle);
            }
      QVERIFY(foundImage);
      font.setPointSize(18);
      auto& newFont = cache.get(markdown, font, 120);
      QVERIFY(original.isNull());
      GUI::ChatDocumentCache freshFont(emoticons);
      QCOMPARE(newFont.size(), freshFont.get(markdown, font, 120).size());
      QPointer<QTextDocument> previous = &newFont;
      auto& edited = cache.get("Replacement message", font, 500);
      QCOMPARE(edited.toPlainText(), QString("Replacement message"));
      QVERIFY(&edited != previous.data());
   }

   void chatDocumentsEvictAndHandleLargeMessages()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      GUI::ChatDocumentCache cache(emoticons);
      const QFont font = QApplication::font();
      QPointer<QTextDocument> oldest = &cache.get("oldest", font, 400);
      QPointer<QTextDocument> recent = &cache.get("recent", font, 400);
      for (int i = 0; i < 150; ++i)
      {
         cache.get(QString::number(i), font, 400);
         QCOMPARE(&cache.get("recent", font, 400), recent.data());
      }
      QVERIFY(oldest.isNull());
      QVERIFY(!recent.isNull());
      QCOMPARE(cache.get("oldest", font, 400).toPlainText(), QString("oldest"));

      // Long sources also consume the budget, even below the entry-count limit.
      QPointer<QTextDocument> large = &cache.get(QString(220000, 'a'), font, 400);
      cache.get(QString(220000, 'b'), font, 400);
      cache.get(QString(220000, 'c'), font, 400);
      QVERIFY(large.isNull());

      QPointer<QTextDocument> small = &cache.get("small", font, 400);
      QPointer<QTextDocument> oversized = &cache.get(QString(600000, 'x'), font, 400);
      QCOMPARE(oversized->characterCount(), 600001);
      QCOMPARE(&cache.get("small", font, 400), small.data());
      QVERIFY(oversized.isNull());
   }

   void chatSizeCacheTracksFontWidthAndFormattedContent()
   {
      Fixture f;
      auto* view = f.widget.findChild<QTableView*>("tblChat");
      QVERIFY(view);
      const auto index = view->model()->index(0, 0);
      GUI::ChatDocumentCache fresh(f.emoticons);
      QStyleOptionViewItem option;
      option.initFrom(view);
      option.widget = view;
      for (const auto& locale : { QLocale(QLocale::English), QLocale(QLocale::German) })
      {
         SETTINGS.set("language", locale);
         for (const int width : { 90, 400 })
            for (const int fontSize : { 10, 24 })
            {
               option.rect = QRect(0, 0, width, 100);
               option.font.setPointSize(fontSize);
               const QSize expected(width, qCeil(fresh.get(index.data().toString(), option.font, width).size().height()));
               QCOMPARE(view->itemDelegate()->sizeHint(option, index), expected);
               QCOMPARE(view->itemDelegate()->sizeHint(option, index), expected);
            }
      }
      SETTINGS.set("language", QLocale::system());
   }

   void chatDocumentsInvalidateEmoticonTheme()
   {
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      for (const QString& name : { QString("first"), QString("second") })
      {
         const QString path = directory.path() + '/' + name;
         QVERIFY(QDir().mkpath(path));
         QImage smile(16, 16, QImage::Format_ARGB32);
         smile.fill(Qt::red);
         QVERIFY(smile.save(path + "/smile.png"));
         QFile metadata(path + "/Emoticons.plist");
         QVERIFY(metadata.open(QIODevice::WriteOnly));
         const QByteArray xml = "<plist><dict><key>Emoticons</key><dict><key>smile.png</key><dict><key>Equivalents</key><array><string>:)</string></array></dict></dict></dict></plist>";
         QCOMPARE(metadata.write(xml), xml.size());
      }
      GUI::Emoticons emoticons(directory.path(), "first");
      QCOMPARE(emoticons.getDefaultTheme(), QString("first"));
      GUI::ChatDocumentCache cache(emoticons);
      const QFont font = QApplication::font();
      const QString markdown = "![smile](emoticons://first/smile.png)";
      QPointer<QTextDocument> previous = &cache.get(markdown, font, 200);
      QVERIFY(!previous->resource(QTextDocument::ImageResource, QUrl("emoticons://first/smile.png")).value<QPixmap>().isNull());
      emoticons.setDefaultTheme("second");
      QCOMPARE(emoticons.getDefaultTheme(), QString("second"));
      auto& updated = cache.get(markdown, font, 200);
      QVERIFY(previous.isNull());
      QVERIFY(!updated.resource(QTextDocument::ImageResource, QUrl("emoticons://first/smile.png")).value<QPixmap>().isNull());
   }

   void chatLinksFollowPaintedLayout()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      GUI::ChatDelegate delegate(emoticons);
      QStandardItemModel model(1, 1);
      const QString markdown = "Soft\nwrap with an ![image](emoticons://missing/smile) and [click here](https://example.com).";
      model.setData(model.index(0, 0), markdown);
      QTableView view;
      view.setModel(&model);
      view.setItemDelegate(&delegate);
      view.horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
      view.verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
      view.setShowGrid(false);
      view.resize(400, 250);
      view.show();
      for (const int fontSize : { 10, 20 })
      {
         QFont font = view.font();
         font.setPointSize(fontSize);
         view.setFont(font);
         view.resizeRowsToContents();
         QApplication::processEvents();
         const auto index = model.index(0, 0);
         QStyleOptionViewItem option;
         option.initFrom(&view);
         option.font = font;
         option.widget = &view;
         option.rect = view.visualRect(index);
         option.features |= QStyleOptionViewItem::HasDisplay;
         QImage rendered(view.viewport()->size(), QImage::Format_ARGB32_Premultiplied);
         rendered.fill(Qt::transparent);
         QPainter painter(&rendered);
         delegate.paint(&painter, option, index);
         painter.end();

         GUI::ChatDocumentCache reference(emoticons);
         auto& document = reference.get(markdown, font, option.rect.width());
         QPoint linkPoint(-1, -1);
         for (int y = 0; y < document.size().height() && linkPoint.x() < 0; ++y)
            for (int x = 0; x < option.rect.width(); ++x)
               if (document.documentLayout()->anchorAt(QPointF(x, y)) == "https://example.com")
               {
                  linkPoint = QPoint(x, y);
                  break;
               }
         QVERIFY(linkPoint.x() >= 0);
         const QRect textRect = view.style()->subElementRect(QStyle::SE_ItemViewItemText, &option, &view);
         const QPoint position = textRect.topLeft() + linkPoint;
         QMouseEvent move(QEvent::MouseMove, QPointF(position), QPointF(view.viewport()->mapToGlobal(position)), Qt::NoButton, Qt::NoButton, Qt::NoModifier);
         delegate.eventFilter(view.viewport(), &move);
         QCOMPARE(view.viewport()->cursor().shape(), Qt::PointingHandCursor);
         QEvent leave(QEvent::Leave);
         delegate.eventFilter(view.viewport(), &leave);
         QCOMPARE(view.viewport()->cursor().shape(), Qt::ArrowCursor);
      }
   }

   void filtersAndAccepts_data()
   {
      QTest::addColumn<int>("acceptKey");
      QTest::newRow("tab") << int(Qt::Key_Tab);
      QTest::newRow("return") << int(Qt::Key_Return);
      QTest::newRow("keypad-enter") << int(Qt::Key_Enter);
   }

   void filtersAndAccepts()
   {
      QFETCH(int, acceptKey);
      Fixture f;
      f.type("@");
      QVERIFY(f.completion->isVisible());
      QCOMPARE(f.list->model()->rowCount(), 3);
      f.type("aL");
      QCOMPARE(f.list->model()->rowCount(), 2);
      f.key(Qt::Key_Down);
      const QString selected = f.list->currentIndex().data().toString();
      f.key(acceptKey);
      QVERIFY(!f.completion->isVisible());
      QCOMPARE(f.editor->toPlainText(), "@" + selected + " ");
      QCOMPARE(f.connection->sentMessages, 0);
      f.type("hello");
      QCOMPARE(f.editor->toPlainText(), "@" + selected + " hello");
      f.key(Qt::Key_Return);
      QCOMPARE(f.connection->sentMessages, 1);
      QCOMPARE(f.connection->answers.size(), 1);
   }

   void spaceDismissesAndBackspaceReopens()
   {
      Fixture f;
      f.type("@Al ");
      QVERIFY(!f.completion->isVisible());
      QCOMPARE(f.editor->toPlainText(), QString("@Al "));
      f.key(Qt::Key_Backspace);
      QVERIFY(f.completion->isVisible());
      QCOMPARE(f.list->model()->rowCount(), 2);
      f.key(Qt::Key_Backspace);
      f.key(Qt::Key_Backspace);
      QCOMPARE(f.editor->toPlainText(), QString("@"));
      QCOMPARE(f.list->model()->rowCount(), 3);
      f.key(Qt::Key_Backspace);
      QVERIFY(!f.completion->isVisible());
      QVERIFY(f.editor->toPlainText().isEmpty());
   }

   void noMatchesReopenAfterBackspace()
   {
      Fixture f;
      f.type("@Alxy");
      QVERIFY(!f.completion->isVisible());
      QCOMPARE(f.editor->toPlainText(), QString("@Alxy"));
      f.key(Qt::Key_Tab);
      QCOMPARE(f.editor->toPlainText(), QString("@Alxy"));
      f.key(Qt::Key_Backspace);
      QVERIFY(!f.completion->isVisible());
      f.key(Qt::Key_Backspace);
      QVERIFY(f.completion->isVisible());
      QCOMPARE(f.list->model()->rowCount(), 2);
      QCOMPARE(f.editor->toPlainText(), QString("@Al"));
   }

   void tabAndEscapePreserveSurroundingText()
   {
      Fixture f;
      f.editor->setPlainText("head tail");
      auto cursor = f.editor->textCursor();
      cursor.setPosition(5);
      f.editor->setTextCursor(cursor);
      f.key(Qt::Key_Tab);
      QVERIFY(f.completion->isVisible());
      f.type("Ali");
      f.key(Qt::Key_Escape);
      QCOMPARE(f.editor->toPlainText(), QString("head @Alitail"));
      f.key(Qt::Key_Tab);
      QVERIFY(f.completion->isVisible());
      QCOMPARE(f.list->model()->rowCount(), 1);
      f.key(Qt::Key_Tab);
      QCOMPARE(f.editor->toPlainText(), QString("head @Alice tail"));
      f.type("and ");
      QCOMPARE(f.editor->toPlainText(), QString("head @Alice and tail"));
   }

   void emptyPeerListStaysClosed()
   {
      Fixture f(QStringList{});
      f.type("@");
      QVERIFY(!f.completion->isVisible());
      QCOMPARE(f.editor->toPlainText(), QString("@"));
      f.type("x");
      f.key(Qt::Key_Backspace);
      QVERIFY(!f.completion->isVisible());
   }

   void literalAndUnicodePrefixes()
   {
      const QString emoji = QString::fromUcs4(U"\U0001f600");
      Fixture f({ "A*star", "Alice", emoji + "Peer" });
      f.type("@A*");
      QCOMPARE(f.list->model()->rowCount(), 1);
      f.key(Qt::Key_Return);
      QCOMPARE(f.editor->toPlainText(), QString("@A*star "));
      f.type("@");
      f.key(0, emoji);
      QCOMPARE(f.list->model()->rowCount(), 1);
      f.key(Qt::Key_Backspace);
      QCOMPARE(f.editor->toPlainText(), QString("@A*star @"));
      QCOMPARE(f.list->model()->rowCount(), 3);
      f.key(0, emoji);
      f.type("x");
      QVERIFY(!f.completion->isVisible());
      f.key(Qt::Key_Backspace);
      QVERIFY(f.completion->isVisible());
      QCOMPARE(f.list->model()->rowCount(), 1);
   }
};

int main(int argc, char** argv)
{
   // Qt may report platform warnings before the test data directory is configured.
   qInstallMessageHandler(nullptr);
   QApplication app(argc, argv);
   QTemporaryDir data;
   if (!data.isValid())
      return 1;
   Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, data.path());
   Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, data.path());
   SETTINGS.setSettingsMessage(new Protos::GUI::Settings());
   SETTINGS.set("max_chat_message_displayed", quint32(500));
   TestsChatCompletion tests;
   return QTest::qExec(&tests, argc, argv);
}

#include "TestsChatCompletion.moc"
