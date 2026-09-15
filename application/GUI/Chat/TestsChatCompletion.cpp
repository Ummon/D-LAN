#include <QtTest>
#include <QListView>
#include <QTemporaryDir>

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
