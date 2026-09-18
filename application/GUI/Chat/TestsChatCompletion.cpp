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
#include <QClipboard>
#include <QPushButton>
#include <QMenu>
#include <QTimer>

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
      QString lastMessage;
      QList<Common::Hash> answers;

      QSharedPointer<RCC::ISendChatMessageResult> sendChatMessage(
         const QString& message, const QString&, const QList<Common::Hash>& answers) override
      {
         ++this->sentMessages;
         this->lastMessage = message;
         this->answers = answers;
         return QSharedPointer<PendingResult>::create();
      }
   };

   struct Fixture
   {
      QSharedPointer<Connection> connection = QSharedPointer<Connection>::create();
      GUI::Emoticons emoticons;
      GUI::ChatWidget widget { connection, emoticons };
      GUI::ChatTextEdit* editor = widget.findChild<GUI::ChatTextEdit*>("txtMessage");
      GUI::AutoComplete* completion = widget.findChild<GUI::AutoComplete*>();
      QListView* list = completion->findChild<QListView*>();

      Fixture(const QStringList& peers = { "Alice", "Alex", "Bob" }, const QString& emoticonDirectory = "nonexistent-test-emoticons") :
         emoticons(emoticonDirectory, "default")
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

      void receiveMessage(const QString& text)
      {
         Protos::Common::ChatMessages messages;
         auto* message = messages.add_messages();
         message->set_id(widget.findChild<QTableView*>("tblChat")->model()->rowCount() + 1);
         message->set_time(QDateTime::currentMSecsSinceEpoch());
         message->set_peer_nick("Alice");
         message->set_message(text.toStdString());
         emit connection->newChatMessages(messages);
      }
   };
}

class TestsChatCompletion : public QObject
{
   Q_OBJECT

private slots:
   void copiedHtmlBreaksBecomeNewlines_data()
   {
      QTest::addColumn<QString>("markdown");
      QTest::addColumn<QString>("expected");
      QTest::newRow("consecutive") << QString("first<br/><br/>last") << QString("first\n\nlast");
      QTest::newRow("edges") << QString("<br/>first<br/>") << QString("\nfirst\n");
      QTest::newRow("variants") << QString("first<BR>second<br />third") << QString("first\nsecond\nthird");
      QTest::newRow("inline-code") << QString("first<br/>`<br/>`") << QString("first\n`<br/>`");
      QTest::newRow("code-block") << QString("first<br/>second\n\n```html\n<br/>\n```") << QString("first\nsecond\n\n```html\n<br/>\n```");
      QTest::newRow("escaped") << QString("\\<br/> &lt;br/&gt;") << QString("\\<br/> &lt;br/&gt;");
   }

   void copiedHtmlBreaksBecomeNewlines()
   {
      QFETCH(QString, markdown);
      QFETCH(QString, expected);
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(markdown, emoticons), expected);
   }

   void copyAndPasteMultilineMessage()
   {
      Fixture f({}, QFINDTESTDATA("../resources/emoticons"));
      f.type("first :D");
      QTest::keyClick(f.editor, Qt::Key_Return, Qt::ShiftModifier);
      QTest::keyClick(f.editor, Qt::Key_Return, Qt::ShiftModifier);
      f.type("last :D");
      f.widget.sendMessage();
      f.receiveMessage(f.connection->lastMessage);
      f.widget.findChild<QTableView*>("tblChat")->selectRow(0);
      f.widget.copySelectedMessagesToClipboard();
      QCOMPARE(QApplication::clipboard()->text(), QString("first :D\n\nlast :D\n"));
      f.editor->clear();
      f.editor->paste();
      const QString image(QChar::ObjectReplacementCharacter);
      QCOMPARE(f.editor->toPlainText(), "first " + image + "\n\nlast " + image + '\n');
      f.widget.copySelectedLineToClipboard();
      QVERIFY(QApplication::clipboard()->text().endsWith("*Alice*: first :D\n\nlast :D\n"));
   }

   void copiedEmoticonsStayOnOneLine_data()
   {
      QTest::addColumn<QString>("text");
      for (const QString& text : { ":D :D :D", ":D asd :D", "asd :D asd :D asd", "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen" })
         QTest::newRow(qPrintable(text)) << text;
   }

   void copiedEmoticonsStayOnOneLine()
   {
      QFETCH(QString, text);
      Fixture f({}, QFINDTESTDATA("../resources/emoticons"));
      f.type(text);
      f.widget.sendMessage();
      f.receiveMessage(f.connection->lastMessage);
      f.widget.findChild<QTableView*>("tblChat")->selectRow(0);
      f.widget.copySelectedMessagesToClipboard();
      QCOMPARE(QApplication::clipboard()->text().trimmed(), text);
      f.widget.copySelectedLineToClipboard();
      const QString line = QApplication::clipboard()->text().trimmed();
      QVERIFY2(line.endsWith("*Alice*: " + text), qPrintable(line));
      QVERIFY(!line.contains('\n'));
   }

   void copiedSoftWrapsPreserveMarkdownStructure()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      const QString image = "![\\:D](emoticons://default/biggrin.png)";
      const QString paragraph = image + " **bold**\ntext " + image;
      const QString copiedParagraph = ":D **bold** text :D";
      const QString structure = "\n\n# Heading\n## Subheading\n\n- first\n- second\n\n```text\nfirst\nsecond\n```\n\n> quote\n> next\n\nfirst  \nsecond<br/>third\n\nfourth\\\nfifth";
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(paragraph + structure + "\n\n" + paragraph, emoticons),
         copiedParagraph + QString(structure).replace("<br/>", "\n") + "\n\n" + copiedParagraph);
   }

   void copyingLargeCodeBlock()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      const QString code = "```text\n" + QString("some code that must retain its newline\n").repeated(1000) + "```";
      QElapsedTimer timer;
      timer.start();
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown("before\nwrapped\n\n" + code + "\n\nafter\nwrapped", emoticons),
         "before wrapped\n\n" + code + "\n\nafter wrapped");
      qInfo() << "Copying 1000 code lines took" << timer.elapsed() << "ms";
   }

   void copiedInlineFormattingPreservesHardBreaks()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      const QString body = "first<br/>second";
      const QString underlined = "<u style=\"white-space: pre-wrap\">" + body + "</u>";
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(underlined, emoticons), QString("first\nsecond"));
      const QString bold = "<span style=\"white-space: pre-wrap\"><b>" + body + "</b></span>";
      const QString copied = GUI::EmoticonTextDocument::toClipboardMarkdown(bold, emoticons);
      QCOMPARE(copied, QString("**first\nsecond**"));
      QTextDocument parsed;
      parsed.setMarkdown(copied);
      QVERIFY(parsed.find("first").charFormat().fontWeight() >= QFont::Bold);
      QVERIFY(parsed.find("second").charFormat().fontWeight() >= QFont::Bold);
      const QString code = "`" + underlined + "`";
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(code, emoticons), code);
   }

   void sendingOrdinaryImageDoesNotDuplicateIt()
   {
      Fixture f(QStringList{});
      QTextImageFormat image;
      image.setName("photo.png");
      image.setProperty(QTextFormat::ImageAltText, "photo!");
      f.editor->textCursor().insertImage(image);
      f.widget.sendMessage();
      GUI::EmoticonTextDocument received(f.emoticons);
      received.setMarkdown(f.connection->lastMessage);
      QCOMPARE(received.toPlainText(), QString(QChar::ObjectReplacementCharacter));
      QCOMPARE(received.begin().begin().fragment().charFormat().stringProperty(QTextFormat::ImageAltText), QString("photo!"));
   }

   void copyingAdjacentFormattingPreservesEmphasis_data()
   {
      QTest::addColumn<QString>("markdown");
      for (const QString& text : {
            "<u style=\"white-space: pre-wrap\"><b>one</b></u>**two**",
            "**one**<u style=\"white-space: pre-wrap\"><b>two</b></u>",
            "<u style=\"white-space: pre-wrap\"><b>one</b></u><span style=\"white-space: pre-wrap\"><b>two</b></span>" })
         QTest::newRow(qPrintable(text)) << text;
   }

   void copyingAdjacentFormattingPreservesEmphasis()
   {
      QFETCH(QString, markdown);
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      const QString copied = GUI::EmoticonTextDocument::toClipboardMarkdown(markdown, emoticons);
      QTextDocument parsed;
      parsed.setMarkdown(copied);
      QCOMPARE(parsed.toPlainText(), QString("onetwo"));
      QVERIFY(parsed.find("onetwo").charFormat().fontWeight() >= QFont::Bold);
   }

   void copiedEmoticonsPreserveIntentionalBreaks_data()
   {
      QTest::addColumn<QString>("gap");
      QTest::addColumn<QString>("copiedGap");
      QTest::newRow("soft-wrap") << QString("\n") << QString(" ");
      QTest::newRow("wrap-after-space") << QString(" \n") << QString(" ");
      QTest::newRow("paragraph") << QString("\n\n") << QString("\n\n");
      QTest::newRow("hard-break-spaces") << QString("  \n") << QString("  \n");
      QTest::newRow("hard-break-backslash") << QString("\\\n") << QString("\\\n");
      QTest::newRow("html-break") << QString("<br/>") << QString("\n");
   }

   void copiedEmoticonsPreserveIntentionalBreaks()
   {
      QFETCH(QString, gap);
      QFETCH(QString, copiedGap);
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      const QString image = "![\\:D](emoticons://default/biggrin.png)";
      const QString markdown = image + gap + image;
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(markdown, emoticons), ":D" + copiedGap + ":D");
      const QString code = "```markdown\n" + markdown + "\n```";
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(code, emoticons), code);
   }

   void copySelectedMessagesFromContextMenu()
   {
      Fixture f(QStringList{});
      const QStringList bodies {
         "**First** ![O\\_o](emoticons://missing/andy.png)",
         "Unselected message",
         "[12:34:56] *Body*: message\n\n<u style=\"white-space: pre-wrap\"><b>second</b></u>"
      };
      Protos::Common::ChatMessages messages;
      for (int row = 0; row < bodies.size(); ++row)
      {
         auto* message = messages.add_messages();
         message->set_id(row + 1);
         message->set_time(1000 + row);
         message->set_peer_nick("SenderNick");
         message->set_message(bodies[row].toStdString());
      }
      emit f.connection->newChatMessages(messages);
      auto* view = f.widget.findChild<QTableView*>("tblChat");
      view->clearSelection();
      // Select in reverse order; both copy actions should follow the chat's order.
      for (const int row : { 2, 0 })
         view->selectionModel()->select(view->model()->index(row, 0), QItemSelectionModel::Select | QItemSelectionModel::Rows);
      QStringList labels;
      QTimer::singleShot(0, &f.widget, [&]() {
         if (auto* menu = qobject_cast<QMenu*>(QApplication::activePopupWidget()))
         {
            for (auto* action : menu->actions())
               labels.append(action->text());
            menu->actions().first()->trigger();
            menu->close();
         }
      });
      f.widget.displayContextMenu(QPoint());
      QCOMPARE(labels.mid(0, 2), QStringList({ "Copy selected messages", "Copy selected lines" }));
      QCOMPARE(QApplication::clipboard()->text(), QString("**First** O_o\n[12:34:56] *Body*: message\n\n**second**\n"));

      f.widget.copySelectedLineToClipboard();
      QString expectedLines;
      for (const int row : { 0, 2 })
         expectedLines += GUI::EmoticonTextDocument::toClipboardMarkdown(f.widget.chatModel.getLineStr(row), f.emoticons) + '\n';
      QCOMPARE(QApplication::clipboard()->text(), expectedLines);
      QVERIFY(expectedLines.contains("*SenderNick*:"));
      view->clearSelection();
      f.widget.copySelectedMessagesToClipboard();
      QVERIFY(QApplication::clipboard()->text().isEmpty());
   }

   void formattingAroundEmoticons_data()
   {
      QTest::addColumn<int>("styles");
      QTest::addColumn<QString>("text");
      for (const int styles : { 1, 2, 3, 4, 5, 6, 7 })
         for (const QString& text : { "asd", ":D asd", "asd :D", "asd :D xyz" })
            QTest::newRow(qPrintable(QString::number(styles) + ' ' + text)) << styles << text;
   }

   void formattingAroundEmoticons()
   {
      QFETCH(int, styles);
      QFETCH(QString, text);
      Fixture f({}, QFINDTESTDATA("../resources/emoticons"));
      f.widget.findChild<QPushButton*>("butBold")->setChecked(styles & 1);
      f.widget.findChild<QPushButton*>("butItalic")->setChecked(styles & 2);
      f.widget.findChild<QPushButton*>("butUnderline")->setChecked(styles & 4);
      f.type(text);
      const QString expected = QString(text).replace(":D", QString(QChar::ObjectReplacementCharacter));
      QCOMPARE(f.editor->toPlainText(), expected);
      const QString draft = f.editor->toHtml();
      f.key(Qt::Key_Return);
      QCOMPARE(f.connection->sentMessages, 1);
      QCOMPARE(f.editor->toHtml(), draft);
      GUI::EmoticonTextDocument received(f.emoticons);
      received.setMarkdown(f.connection->lastMessage);
      QVERIFY2(received.toPlainText() == expected, qPrintable(f.connection->lastMessage));
      for (const QString& word : { "asd", "xyz" })
      {
         const auto cursor = received.find(word);
         if (cursor.isNull())
            continue;
         QCOMPARE(cursor.charFormat().fontWeight() >= QFont::Bold, bool(styles & 1));
         QCOMPARE(cursor.charFormat().fontItalic(), bool(styles & 2));
         QCOMPARE(cursor.charFormat().fontUnderline(), bool(styles & 4));
      }
      const QString copied = GUI::EmoticonTextDocument::toClipboardMarkdown(f.connection->lastMessage, f.emoticons);
      QVERIFY2(!copied.contains('<'), qPrintable(copied));
      QString expectedMarkdown = text;
      const QString emphasis = QString(styles & 1 ? "**" : "") + (styles & 2 ? "*" : "");
      for (const QString& word : { "asd", "xyz" })
         expectedMarkdown.replace(word, emphasis + word + emphasis);
      QCOMPARE(copied.trimmed(), expectedMarkdown);
      QTextDocument parsedCopy;
      parsedCopy.setMarkdown(copied);
      QCOMPARE(parsedCopy.toPlainText(), text);
      const auto format = parsedCopy.find("asd").charFormat();
      QCOMPARE(format.fontWeight() >= QFont::Bold, bool(styles & 1));
      QCOMPARE(format.fontItalic(), bool(styles & 2));
      QVERIFY(!format.fontUnderline()); // No standard Markdown underline syntax.
   }

   void copiedInlineFormattingKeepsCodeAndWhitespace()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      const QString html = "<span style=\"white-space: pre-wrap\"><b> asd</b></span>";
      const QString code = "`" + html + "`\n\n```html\n" + html + "\n```\n\n    " + html;
      const QString message = "[00:37:05] *Greg*: ![\\:D](emoticons://default/biggrin.png)" + html;
      const QString copied = GUI::EmoticonTextDocument::toClipboardMarkdown(message + "\n\n" + code, emoticons);
      QCOMPARE(copied, "[00:37:05] *Greg*: :D **asd**\n\n" + code);
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(
         "<span style=\"white-space: pre-wrap\"><s><b><i>  asd  </i></b></s></span>", emoticons), QString("  ~~***asd***~~  "));
      const QString underline = "<u style=\"white-space: pre-wrap\">asd</u>";
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(underline + " `" + underline + "`", emoticons), "asd `" + underline + "`");
   }

   void emoticonAltTextEscapesMarkdown_data()
   {
      QTest::addColumn<QString>("symbol");
      for (const QString& symbol : { "O_o", ";]", ":[", ":[[", ":\\", ":-\\", "(*_*)", "*w*", ":'(", ">:)", "<3" })
         QTest::newRow(qPrintable(symbol)) << symbol;
   }

   void emoticonAltTextEscapesMarkdown()
   {
      QFETCH(QString, symbol);
      Fixture f({}, QFINDTESTDATA("../resources/emoticons"));
      f.editor->insertPlainText(symbol);
      QCOMPARE(f.editor->toPlainText(), QString(QChar::ObjectReplacementCharacter));
      f.widget.sendMessage();
      GUI::EmoticonTextDocument received(f.emoticons);
      received.setMarkdown(f.connection->lastMessage);
      QCOMPARE(received.toPlainText(), QString(QChar::ObjectReplacementCharacter));
      const auto format = received.begin().begin().fragment().charFormat();
      QVERIFY2(format.isImageFormat(), qPrintable(f.connection->lastMessage));
      QCOMPARE(format.stringProperty(QTextFormat::ImageAltText), symbol);

      f.receiveMessage(f.connection->lastMessage);
      f.widget.findChild<QTableView*>("tblChat")->selectRow(0);
      f.widget.copySelectedLineToClipboard();
      const QString copied = QApplication::clipboard()->text();
      QVERIFY2(copied.trimmed().endsWith("*Alice*: " + symbol), qPrintable(copied));
   }

   void emoticonSymbolsSurviveSendCopyAndPaste()
   {
      Fixture f({}, QFINDTESTDATA("../resources/emoticons"));
      const QString symbols = "O_o o_O :) :-) <3";
      f.type(symbols);
      QCOMPARE(f.editor->toPlainText().count(QChar::ObjectReplacementCharacter), 5);
      f.widget.sendMessage();
      GUI::EmoticonTextDocument received(f.emoticons);
      received.setMarkdown(f.connection->lastMessage);
      QStringList altText;
      for (auto block = received.begin(); block.isValid(); block = block.next())
         for (auto it = block.begin(); !it.atEnd(); ++it)
            if (it.fragment().charFormat().isImageFormat())
               altText.append(it.fragment().charFormat().stringProperty(QTextFormat::ImageAltText));
      QCOMPARE(altText, symbols.split(' '));

      f.receiveMessage("**before** " + f.connection->lastMessage.trimmed() + " [after](https://example.com)");
      auto* view = f.widget.findChild<QTableView*>("tblChat");
      view->selectRow(0);
      f.widget.copySelectedLineToClipboard();
      const QString copied = QApplication::clipboard()->text();
      QVERIFY2(copied.simplified().contains("**before** " + symbols + " [after](https://example.com)"), qPrintable(copied));
      QVERIFY(!copied.contains("emoticons://"));

      f.editor->clear();
      f.editor->paste();
      QCOMPARE(f.editor->toPlainText().count(QChar::ObjectReplacementCharacter), 5);
      const QString pasted = f.editor->toHtml();
      int steps = 0;
      while (f.editor->document()->isUndoAvailable() && ++steps < 20)
         f.editor->undo();
      QVERIFY(f.editor->toPlainText().isEmpty());
      while (f.editor->document()->isRedoAvailable())
         f.editor->redo();
      QCOMPARE(f.editor->toHtml(), pasted);
   }

   void pasteConvertsEveryEmoticonWord()
   {
      Fixture f({}, QFINDTESTDATA("../resources/emoticons"));
      f.editor->insertPlainText("prefix suffix");
      auto cursor = f.editor->textCursor();
      cursor.setPosition(7);
      f.editor->setTextCursor(cursor);
      QApplication::clipboard()->setText("  O_o\t:)\n:-) <3 ");
      f.editor->paste();
      const QString image(QChar::ObjectReplacementCharacter);
      QCOMPARE(f.editor->toPlainText(), "prefix   " + image + '\t' + image + '\n' + image + ' ' + image + " suffix");
      QCOMPARE(f.editor->textCursor().position(), f.editor->toPlainText().indexOf("suffix"));

      f.editor->clear();
      f.editor->insertPlainText("O_");
      f.editor->moveCursor(QTextCursor::End);
      QApplication::clipboard()->setText("o");
      f.editor->paste();
      QCOMPARE(f.editor->toPlainText(), image);
   }

   void pickedEmoticonIncludesDefaultSymbol()
   {
      Fixture f({}, QFINDTESTDATA("../resources/emoticons"));
      f.widget.insertEmoticon("default", "andy.png");
      f.widget.sendMessage();
      GUI::EmoticonTextDocument received(f.emoticons);
      received.setMarkdown(f.connection->lastMessage);
      const auto format = received.begin().begin().fragment().charFormat();
      QVERIFY(format.isImageFormat());
      QCOMPARE(format.stringProperty(QTextFormat::ImageAltText), QString("o_O"));
   }

   void copyingEmoticonsPreservesOtherMarkdown()
   {
      GUI::Emoticons emoticons(QFINDTESTDATA("../resources/emoticons"), "default");
      const QString link = "![O\\_o](emoticons://missing/andy.png)";
      const QString literal = "`" + link + "`\n\n```markdown\n" + link + "\n```\n\n    " + link;
      const QString surrounding = "**bold** [link](https://example.com) ![photo](photo.png) &#x20;\n\n";
      const QString markdown = surrounding + link + "\n\n" + literal;
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(markdown, emoticons), surrounding + "O_o\n\n" + literal);
      GUI::EmoticonTextDocument rendered(emoticons);
      rendered.setMarkdown(markdown);
      QCOMPARE(rendered.toPlainText().count(QChar::ObjectReplacementCharacter), 2);
      QCOMPARE(rendered.toPlainText().count(link), 3);
      QVERIFY(!rendered.toPlainText().contains("DLANEMOTICON"));

      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown("![image](emoticons://default/andy.png)", emoticons), QString("o_O"));
      const QString unknown = "![image](emoticons://missing/andy.png)";
      QCOMPARE(GUI::EmoticonTextDocument::toClipboardMarkdown(unknown, emoticons), unknown);
   }

   void emoticonMarkersDoNotReplaceUserText()
   {
      GUI::Emoticons emoticons("nonexistent-test-emoticons");
      GUI::EmoticonTextDocument document(emoticons);
      const QString literal = "dlanemoticon0";
      document.setMarkdown(literal + " ![O\\_o](emoticons://missing/andy.png)");
      QCOMPARE(document.toPlainText(), literal + ' ' + QChar::ObjectReplacementCharacter);
      const QString code = "![O\\_o](emoticons://missing/andy.png)";
      document.setMarkdown(literal + " `" + code + "`");
      QCOMPARE(document.toPlainText(), literal + ' ' + code);
   }

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
