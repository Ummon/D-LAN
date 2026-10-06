#include <memory>

#include <QDir>
#include <QTest>
#include <QTemporaryDir>

#include <Common/Global.h>
#include <Common/Network/MessageSocket.h>
#include <Common/RemoteCoreController/Builder.h>
#include <Common/RemoteCoreController/IBrowseResult.h>
#include <Common/RemoteCoreController/ISearchResult.h>
#include <Common/RemoteCoreController/ISendChatMessageResult.h>

#include <Server.h>
#include <State.h>

using DummyCore::State;

namespace
{
   const QByteArray JSON = R"({
      "peers": [
         {
            "name": "Bob", "sharing": "2 GiB", "download_rate": 10, "version": "1.2.3",
            "shared": [
               { "name": "Empty", "children": [] },
               { "name": "Files", "children": [
                  { "name": "b.txt", "size": 1 },
                  { "name": "Music", "children": [
                     { "name": "song.mp3", "size": "1.5 MiB" },
                     { "name": "Album", "children": [ { "name": "A.mp3", "size": 1024 } ] }
                  ] },
                  { "name": "a.txt", "size": "2 KiB" },
                  { "name": "Zoo", "children": [] }
               ] }
            ]
         },
         {
            "name": "Alice", "self": true, "download_rate": "1.5 MiB/s", "upload_rate": "2 KiB",
            "shared": [ { "name": "Incoming", "children": [ { "name": "x", "size": 5 } ] } ]
         },
         { "name": "Carol" }
      ],
      "search": [
         { "peer": "Bob", "path": "Files/Music", "level": 2 },
         { "peer": "Alice", "path": "Incoming/x" },
         { "peer": "Bob", "path": "/Files/Music/Album/A.mp3" }
      ],
      "downloads": [
         { "path": "Music/Album", "name": "A.mp3", "size": "3.7 GiB", "status": "DOWNLOADING", "progress": 19.1, "peer": "Bob", "other_peers": ["Carol"] },
         { "name": "done.iso", "size": 12345, "status": "complete", "peer": "Carol" },
         { "name": "queued.iso", "size": 10, "peer": "Bob" }
      ],
      "uploads": [
         { "path": "/Videos/Cats/", "name": "cat.avi", "size": "1 MiB", "progress": 80.14, "peer": "Carol" }
      ]
   })";

   // A valid beginning of document for the tests of the other parts.
   const QByteArray PEERS = R"("peers": [ { "name": "a", "self": true, "shared": [ { "name": "S", "children": [ { "name": "f", "size": 1 } ] } ] } ])";

   Common::Hash hash(const Protos::Common::Hash& hash)
   {
      return Common::Hash(hash.hash());
   }

   QString str(const std::string& string)
   {
      return QString::fromStdString(string);
   }
}

class Tests : public QObject
{
   Q_OBJECT

   QTemporaryDir directory;
   State state;
   Common::Hash bob;

   Protos::GUI::BrowseResult browseState(const Common::Hash& peerID, const QList<Protos::Common::Entry>& directories = {}, bool roots = false) const
   {
      Protos::GUI::Browse browse;
      browse.mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
      for (const auto& directory : directories)
         browse.mutable_dirs()->add_entries()->CopyFrom(directory);
      browse.set_get_roots(roots);
      browse.set_tag(42);
      return this->state.browse(browse);
   }

private slots:
   void initTestCase()
   {
      // The logs of the core connection.
      QVERIFY(this->directory.isValid());
      Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, this->directory.path());
      Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, this->directory.path());
   }

   void cleanupTestCase()
   {
      Common::Global::setDataFolderToDefault(Common::Global::DataFolderType::ROAMING);
      Common::Global::setDataFolderToDefault(Common::Global::DataFolderType::LOCAL);
   }

   void init()
   {
      QCOMPARE(this->state.loadFromJson(JSON), QString());
      this->bob = hash(this->state.getState().peers(1).peer_id());
   }

   void peers()
   {
      const Protos::GUI::State& state = this->state.getState();
      QCOMPARE(state.peers_size(), 3);

      // "The first peer is always ourself".
      QCOMPARE(str(state.peers(0).nick()), QString("Alice"));
      QVERIFY(!this->state.getSelfID().isNull());
      QCOMPARE(hash(state.peers(0).peer_id()), this->state.getSelfID());
      QCOMPARE(state.peers(0).sharing_amount(), quint64(5)); // The size of its shared folders.
      QCOMPARE(state.peers(0).download_rate(), quint64(1572864));
      QCOMPARE(state.peers(0).upload_rate(), quint64(2048));
      QCOMPARE(str(state.peers(0).core_version()), Common::Global::getVersionFull());

      QCOMPARE(str(state.peers(1).nick()), QString("Bob"));
      QCOMPARE(state.peers(1).sharing_amount(), quint64(2147483648));
      QCOMPARE(state.peers(1).download_rate(), quint64(10));
      QCOMPARE(state.peers(1).upload_rate(), quint64(0));
      QCOMPARE(str(state.peers(1).core_version()), QString("1.2.3"));
      QCOMPARE(state.peers(1).status(), Protos::GUI::State::Peer::OK);

      QCOMPARE(str(state.peers(2).nick()), QString("Carol"));
      QCOMPARE(state.peers(2).sharing_amount(), quint64(0));
      QVERIFY(hash(state.peers(2).peer_id()) != this->bob);

      // The status bar.
      QCOMPARE(state.stats().cache_status(), Protos::GUI::State::Stats::UP_TO_DATE);
      QCOMPARE(state.stats().download_rate(), quint64(1572864));
      QCOMPARE(state.stats().upload_rate(), quint64(2048));

      // The same file always gives the same IDs.
      State other;
      QCOMPARE(other.loadFromJson(JSON), QString());
      QCOMPARE(other.getState().SerializeAsString(), state.SerializeAsString());
   }

   void downloads()
   {
      const Protos::GUI::State& state = this->state.getState();
      const Common::Hash carol = hash(state.peers(2).peer_id());
      QCOMPARE(state.downloads_size(), 3);

      const auto& downloading = state.downloads(0);
      QCOMPARE(downloading.id(), quint64(1));
      QCOMPARE(downloading.local_entry().type(), Protos::Common::Entry::FILE);
      QCOMPARE(str(downloading.local_entry().path()), QString("Music/Album/"));
      QCOMPARE(str(downloading.local_entry().name()), QString("A.mp3"));
      QCOMPARE(downloading.local_entry().size(), quint64(3972844749));
      QVERIFY(downloading.local_entry().exists());
      QCOMPARE(downloading.status(), Protos::Common::DOWNLOADING);
      // As 'GUI::DownloadsModel' computes the progress.
      QCOMPARE(10000 * downloading.downloaded_bytes() / downloading.local_entry().size(), quint64(1910));
      QCOMPARE(downloading.peer_ids_size(), 2);
      QCOMPARE(hash(downloading.peer_ids(0)), this->bob);
      QCOMPARE(hash(downloading.peer_ids(1)), carol);
      QCOMPARE(str(downloading.peer_source_nick()), QString("Bob"));

      const auto& complete = state.downloads(1);
      QCOMPARE(complete.id(), quint64(2));
      QVERIFY(complete.local_entry().path().empty());
      QCOMPARE(complete.status(), Protos::Common::COMPLETE);
      QCOMPARE(complete.downloaded_bytes(), quint64(12345));
      QVERIFY(complete.local_entry().exists());
      QCOMPARE(complete.peer_ids_size(), 1);
      QCOMPARE(hash(complete.peer_ids(0)), carol);

      const auto& queued = state.downloads(2);
      QCOMPARE(queued.status(), Protos::Common::QUEUED);
      QCOMPARE(queued.downloaded_bytes(), quint64(0));
      QVERIFY(!queued.local_entry().exists());
   }

   void uploads()
   {
      const Protos::GUI::State& state = this->state.getState();
      QCOMPARE(state.uploads_size(), 1);
      QCOMPARE(state.uploads(0).id(), quint64(1));
      QCOMPARE(state.uploads(0).file().type(), Protos::Common::Entry::FILE);
      QCOMPARE(str(state.uploads(0).file().path()), QString("Videos/Cats/"));
      QCOMPARE(str(state.uploads(0).file().name()), QString("cat.avi"));
      QCOMPARE(state.uploads(0).file().size(), quint64(1048576));
      QCOMPARE(state.uploads(0).progress(), quint32(8014));
      QCOMPARE(hash(state.uploads(0).peer_id()), hash(state.peers(2).peer_id()));
   }

   void sizes_data()
   {
      QTest::addColumn<QByteArray>("json");
      QTest::addColumn<quint64>("size");
      QTest::newRow("number") << QByteArray("1024") << quint64(1024);
      QTest::newRow("bytes") << QByteArray(R"("670 B")") << quint64(670);
      QTest::newRow("no-unit") << QByteArray(R"("670")") << quint64(670);
      QTest::newRow("kib") << QByteArray(R"("198.0 KiB")") << quint64(202752);
      QTest::newRow("no-space") << QByteArray(R"("1.5GiB")") << quint64(1610612736);
      QTest::newRow("case-and-spaces") << QByteArray(R"(" 2 tib ")") << quint64(2199023255552);
      QTest::newRow("pib") << QByteArray(R"("0.5 PiB")") << quint64(562949953421312);
   }

   void sizes()
   {
      QFETCH(QByteArray, json);
      QFETCH(quint64, size);
      State state;
      QCOMPARE(state.loadFromJson(R"({ "peers": [ { "name": "a", "self": true, "sharing": )" + json + " } ] }"), QString());
      QCOMPARE(state.getState().peers(0).sharing_amount(), size);
   }

   /**
     * A rate isn't limited to 32 bits.
     */
   void rateBeyond4GiBPerSecond()
   {
      State state;
      QCOMPARE(state.loadFromJson(R"({ "peers": [ { "name": "a", "self": true, "download_rate": "5 GiB/s", "upload_rate": 6000000000 } ] })"), QString());
      QCOMPARE(state.getState().peers(0).download_rate(), quint64(5368709120));
      QCOMPARE(state.getState().peers(0).upload_rate(), quint64(6000000000));
      QCOMPARE(state.getState().stats().download_rate(), quint64(5368709120));
      QCOMPARE(state.getState().stats().upload_rate(), quint64(6000000000));
   }

   void errors_data()
   {
      QTest::addColumn<QByteArray>("json");
      QTest::addColumn<QString>("error");

      const auto peer = [](const QByteArray& values) -> QByteArray { return R"({ "peers": [ { "name": "a", "self": true, )" + values + " } ] }"; };
      const auto with = [](const QByteArray& values) -> QByteArray { return "{ " + PEERS + ", " + values + " }"; };

      QTest::newRow("not-json") << QByteArray("{\n\"peers\": [\n}") << "(line 3)";
      QTest::newRow("not-an-object") << QByteArray("[]") << "must be a JSON object";
      QTest::newRow("no-self") << QByteArray(R"({ "peers": [ { "name": "a" } ] })") << "must be the dummy Core itself";
      QTest::newRow("two-selves") << QByteArray(R"({ "peers": [ { "name": "a", "self": true }, { "name": "b", "self": true } ] })") << "'peers[1].self': only one peer";
      QTest::newRow("same-peer-names") << QByteArray(R"({ "peers": [ { "name": "a", "self": true }, { "name": "a" } ] })") << "already a peer named 'a'";
      QTest::newRow("no-peer-name") << QByteArray(R"({ "peers": [ { "self": true } ] })") << "'peers[0].name' must be defined";
      QTest::newRow("peer-not-an-object") << QByteArray(R"({ "peers": [ "a" ] })") << "'peers[0]' must be an object";
      QTest::newRow("peers-not-an-array") << QByteArray(R"({ "peers": {} })") << "'peers' must be an array";
      QTest::newRow("unknown-key") << peer(R"("nick": "x")") << "Unknown key 'peers[0].nick'";
      QTest::newRow("unknown-top-key") << with(R"("chat": [])") << "Unknown key 'chat'";
      QTest::newRow("unknown-unit") << peer(R"("sharing": "1.5 GB")") << "'peers[0].sharing' must be a number of bytes";
      QTest::newRow("negative-size") << peer(R"("sharing": -1)") << "'peers[0].sharing' must be a number of bytes";
      QTest::newRow("negative-rate") << peer(R"("download_rate": "-1 KiB/s")") << "'peers[0].download_rate' must be a number of bytes";
      QTest::newRow("self-not-a-boolean") << QByteArray(R"({ "peers": [ { "name": "a", "self": 1 } ] })") << "'peers[0].self' must be true or false";
      QTest::newRow("slash-in-name") << peer(R"("shared": [ { "name": "S", "children": [ { "name": "a/b" } ] } ])") << "'peers[0].shared[0].children[0].name' can't contain a '/'";
      QTest::newRow("same-names") << peer(R"("shared": [ { "name": "S", "children": [ { "name": "f" }, { "name": "F", "children": [] } ] } ])") << "'peers[0].shared[0].children[1].name': there is already an entry named 'F'";
      QTest::newRow("same-shared-folders") << peer(R"("shared": [ { "name": "S" }, { "name": "s" } ])") << "already a shared folder named 's'";
      QTest::newRow("directory-size") << peer(R"("shared": [ { "name": "S", "children": [ { "name": "d", "size": 1, "children": [] } ] } ])") << "'peers[0].shared[0].children[0].size': the size of a directory";
      QTest::newRow("search-unknown-peer") << with(R"("search": [ { "peer": "b", "path": "S/f" } ])") << "'search[0].peer' must be the name of a peer";
      QTest::newRow("search-unknown-path") << with(R"("search": [ { "peer": "a", "path": "S/g" } ])") << "'search[0].path': 'S/g' isn't shared by the peer 'a'";
      QTest::newRow("search-level") << with(R"("search": [ { "peer": "a", "path": "S/f", "level": 1.5 } ])") << "'search[0].level' must be an integer";
      QTest::newRow("download-status") << with(R"("downloads": [ { "name": "f", "peer": "a", "status": "foo" } ])") << "'downloads[0].status': unknown status 'foo'";
      QTest::newRow("download-progress") << with(R"("downloads": [ { "name": "f", "peer": "a", "progress": 101 } ])") << "'downloads[0].progress' must be a number from 0 to 100";
      QTest::newRow("download-no-peer") << with(R"("downloads": [ { "name": "f" } ])") << "'downloads[0].peer' must be the name of a peer";
      QTest::newRow("download-other-peers") << with(R"("downloads": [ { "name": "f", "peer": "a", "other_peers": ["a", "b"] } ])") << "'downloads[0].other_peers[1]' must be the name of a peer";
      QTest::newRow("upload-no-name") << with(R"("uploads": [ { "peer": "a" } ])") << "'uploads[0].name' must be defined";
      QTest::newRow("upload-status") << with(R"("uploads": [ { "name": "f", "peer": "a", "status": "QUEUED" } ])") << "Unknown key 'uploads[0].status'";
   }

   void errors()
   {
      QFETCH(QByteArray, json);
      QFETCH(QString, error);
      const QString result = this->state.loadFromJson(json);
      QVERIFY2(result.contains(error), qPrintable(result));

      // Nothing remains of the previous state.
      QCOMPARE(this->state.getState().peers_size(), 0);
      QVERIFY(this->state.getSelfID().isNull());
      QCOMPARE(this->browseState(this->bob).entries_size(), 0);
   }

   void browse()
   {
      // The shared folders.
      const Protos::GUI::BrowseResult roots = this->browseState(this->bob);
      QCOMPARE(roots.tag(), quint64(42));
      QCOMPARE(roots.entries_size(), 1);
      QCOMPARE(roots.entries(0).entries_size(), 2);

      const Protos::Common::Entry& empty = roots.entries(0).entries(0);
      QCOMPARE(empty.type(), Protos::Common::Entry::DIR);
      QVERIFY(empty.path().empty());
      QVERIFY(empty.name().empty());
      QCOMPARE(str(empty.shared_entry().shared_name()), QString("Empty"));
      QVERIFY(!hash(empty.shared_entry().id()).isNull());
      QCOMPARE(empty.size(), quint64(0));
      QVERIFY(empty.is_empty());

      const Protos::Common::Entry& files = roots.entries(0).entries(1);
      QCOMPARE(str(files.shared_entry().shared_name()), QString("Files"));
      QVERIFY(hash(files.shared_entry().id()) != hash(empty.shared_entry().id()));
      QCOMPARE(files.size(), quint64(1 + 1572864 + 1024 + 2048));
      QVERIFY(!files.is_empty());

      // The directories first, sorted without regard to case.
      const Protos::GUI::BrowseResult filesContent = this->browseState(this->bob, { files });
      QCOMPARE(filesContent.entries_size(), 1);
      QCOMPARE(filesContent.entries(0).entries_size(), 4);
      const Protos::Common::Entry& music = filesContent.entries(0).entries(0);
      QCOMPARE(str(music.name()), QString("Music"));
      QCOMPARE(music.type(), Protos::Common::Entry::DIR);
      QVERIFY(music.path().empty()); // Relative to the shared folder.
      QCOMPARE(music.size(), quint64(1572864 + 1024));
      QVERIFY(!music.is_empty());
      QCOMPARE(hash(music.shared_entry().id()), hash(files.shared_entry().id()));
      QCOMPARE(str(filesContent.entries(0).entries(1).name()), QString("Zoo"));
      QVERIFY(filesContent.entries(0).entries(1).is_empty());
      QCOMPARE(str(filesContent.entries(0).entries(2).name()), QString("a.txt"));
      QCOMPARE(filesContent.entries(0).entries(2).type(), Protos::Common::Entry::FILE);
      QCOMPARE(filesContent.entries(0).entries(2).size(), quint64(2048));
      QCOMPARE(str(filesContent.entries(0).entries(3).name()), QString("b.txt"));

      const Protos::GUI::BrowseResult musicContent = this->browseState(this->bob, { music });
      QCOMPARE(musicContent.entries(0).entries_size(), 2);
      const Protos::Common::Entry& album = musicContent.entries(0).entries(0);
      QCOMPARE(str(album.name()), QString("Album"));
      QCOMPARE(str(album.path()), QString("Music/"));
      QCOMPARE(album.size(), quint64(1024));
      QCOMPARE(str(musicContent.entries(0).entries(1).name()), QString("song.mp3"));
      QCOMPARE(str(musicContent.entries(0).entries(1).path()), QString("Music/"));

      const Protos::GUI::BrowseResult albumContent = this->browseState(this->bob, { album });
      QCOMPARE(albumContent.entries(0).entries_size(), 1);
      QCOMPARE(str(albumContent.entries(0).entries(0).name()), QString("A.mp3"));
      QCOMPARE(str(albumContent.entries(0).entries(0).path()), QString("Music/Album/"));

      // Several directories then the shared folders, as 'GUI::BrowseModel::refresh()' asks them.
      Protos::Common::Entry unknown;
      unknown.set_type(Protos::Common::Entry::DIR);
      unknown.set_name("Unknown");
      unknown.mutable_shared_entry()->CopyFrom(files.shared_entry());
      const Protos::GUI::BrowseResult several = this->browseState(this->bob, { music, unknown, empty }, true);
      QCOMPARE(several.entries_size(), 4);
      QCOMPARE(several.entries(0).entries_size(), 2);
      QCOMPARE(several.entries(1).entries_size(), 0);
      QCOMPARE(several.entries(2).entries_size(), 0);
      QCOMPARE(several.entries(3).entries_size(), 2);

      // The same directory name in the shared folder of another peer.
      QCOMPARE(this->browseState(this->state.getSelfID(), { music }).entries(0).entries_size(), 0);
      QCOMPARE(this->browseState(this->state.getSelfID()).entries(0).entries_size(), 1);

      // A peer without shared folder and an unknown one.
      QCOMPARE(this->browseState(hash(this->state.getState().peers(2).peer_id())).entries(0).entries_size(), 0);
      QCOMPARE(this->browseState(Common::Hash::rand()).entries_size(), 0);
   }

   void search()
   {
      const QList<Protos::Common::FindResult>& results = this->state.getSearchResults();
      QCOMPARE(results.size(), 2); // One per peer.

      QCOMPARE(hash(results[0].peer_id()), this->bob);
      QCOMPARE(results[0].entries_size(), 2);
      QCOMPARE(results[0].entries(0).level(), quint32(2));
      QCOMPARE(str(results[0].entries(0).entry().name()), QString("Music"));
      QCOMPARE(results[0].entries(0).entry().type(), Protos::Common::Entry::DIR);
      QCOMPARE(results[0].entries(0).entry().size(), quint64(1572864 + 1024));
      QCOMPARE(str(results[0].entries(0).entry().shared_entry().shared_name()), QString("Files"));
      QCOMPARE(results[0].entries(1).level(), quint32(0));
      QCOMPARE(str(results[0].entries(1).entry().name()), QString("A.mp3"));
      QCOMPARE(str(results[0].entries(1).entry().path()), QString("Music/Album/"));

      QCOMPARE(hash(results[1].peer_id()), this->state.getSelfID());
      QCOMPARE(results[1].entries_size(), 1);
      QCOMPARE(str(results[1].entries(0).entry().name()), QString("x"));

      // The GUI can expand a directory of the result.
      QCOMPARE(this->browseState(this->bob, { results[0].entries(0).entry() }).entries(0).entries_size(), 2);
   }

   /**
     * One example per language of the GUI: the same state, only the peer nicks and some folder names differ.
     */
   void examples_data()
   {
      QTest::addColumn<QString>("filename");
      const QStringList filenames = QDir(DUMMY_CORE_EXAMPLES_DIRECTORY).entryList({ "DummyCore.example*.json" }, QDir::Files, QDir::Name);
      QVERIFY(filenames.contains("DummyCore.example.json"));
      for (const QString& filename : filenames)
         QTest::newRow(qPrintable(filename)) << filename;
   }

   void examples()
   {
      QFETCH(QString, filename);
      const QDir directory(DUMMY_CORE_EXAMPLES_DIRECTORY);

      State reference;
      QCOMPARE(reference.load(directory.filePath("DummyCore.example.json")), QString());
      const Protos::GUI::State& expected = reference.getState();
      QCOMPARE(expected.peers_size(), 5);
      QCOMPARE(str(expected.peers(0).nick()), QString("Renoir"));
      QVERIFY(expected.downloads_size() > 0);
      QVERIFY(expected.uploads_size() > 0);
      QCOMPARE(reference.getSearchResults().size(), 1);

      State example;
      QCOMPARE(example.load(directory.filePath(filename)), QString());
      const Protos::GUI::State& state = example.getState();

      QCOMPARE(state.peers_size(), expected.peers_size());
      for (int i = 0; i < state.peers_size(); i++)
      {
         QVERIFY(!state.peers(i).nick().empty());
         QCOMPARE(state.peers(i).sharing_amount(), expected.peers(i).sharing_amount());
         QCOMPARE(state.peers(i).download_rate(), expected.peers(i).download_rate());
         QCOMPARE(state.peers(i).upload_rate(), expected.peers(i).upload_rate());
      }

      QCOMPARE(state.downloads_size(), expected.downloads_size());
      for (int i = 0; i < state.downloads_size(); i++)
      {
         QCOMPARE(state.downloads(i).local_entry().name(), expected.downloads(i).local_entry().name());
         QCOMPARE(state.downloads(i).local_entry().size(), expected.downloads(i).local_entry().size());
         QCOMPARE(state.downloads(i).status(), expected.downloads(i).status());
         QCOMPARE(state.downloads(i).downloaded_bytes(), expected.downloads(i).downloaded_bytes());
         QCOMPARE(state.downloads(i).peer_ids_size(), expected.downloads(i).peer_ids_size());
      }

      QCOMPARE(state.uploads_size(), expected.uploads_size());
      for (int i = 0; i < state.uploads_size(); i++)
      {
         QCOMPARE(state.uploads(i).file().name(), expected.uploads(i).file().name());
         QCOMPARE(state.uploads(i).progress(), expected.uploads(i).progress());
      }

      // The paths of "search" follow the names of the folders.
      QCOMPARE(example.getSearchResults().size(), 1);
      const Protos::Common::FindResult& found = example.getSearchResults().first();
      const Protos::Common::FindResult& expectedFound = reference.getSearchResults().first();
      QCOMPARE(found.entries_size(), expectedFound.entries_size());
      for (int i = 0; i < found.entries_size(); i++)
      {
         QCOMPARE(found.entries(i).level(), expectedFound.entries(i).level());
         QCOMPARE(found.entries(i).entry().name(), expectedFound.entries(i).entry().name());
         QCOMPARE(found.entries(i).entry().size(), expectedFound.entries(i).entry().size());
      }
   }

   void unknownFile()
   {
      State state;
      QVERIFY(state.load(this->directory.filePath("unknown.json")).startsWith("Unable to open the state file"));
   }

   /**
     * With the connection used by the GUI.
     */
   void protocol()
   {
      DummyCore::Server server(this->state);
      QCOMPARE(server.listen(0), QString());
      QVERIFY(server.getPort() != 0);

      const QSharedPointer<RCC::ICoreConnection> connection = RCC::Builder::newCoreConnection(5000);
      connection->setAutoStartLocalCore(false);
      QList<Protos::GUI::State> states;
      connect(connection.data(), &RCC::ICoreConnection::newState, this, [&](const Protos::GUI::State& state) { states << state; });

      // As the GUI does by default, "localhost" may be resolved as an IPv6 and an IPv4 address.
      connection->connectToCore("localhost", server.getPort(), Common::SaltedPassword());
      QTRY_VERIFY(connection->isConnected());
      QVERIFY(connection->isLocal());
      QCOMPARE(connection->getRemoteID(), this->state.getSelfID()); // It's how the GUI knows which peer is the Core.

      // The same state is sent periodically.
      QTRY_VERIFY_WITH_TIMEOUT(states.size() >= 3, 10000);
      for (const Protos::GUI::State& state : std::as_const(states))
         QCOMPARE(state.SerializeAsString(), this->state.getState().SerializeAsString());

      // Any search gives the same result.
      QList<Protos::Common::FindResult> found;
      Protos::Common::FindPattern pattern;
      pattern.set_pattern("anything");
      const QSharedPointer<RCC::ISearchResult> search = connection->search(pattern);
      connect(search.data(), &RCC::ISearchResult::result, this, [&](const Protos::Common::FindResult& result) { found << result; });
      search->start();
      QTRY_COMPARE(found.size(), 2);
      QCOMPARE(hash(found[0].peer_id()), this->bob);
      QCOMPARE(found[0].entries_size(), 2);
      QCOMPARE(hash(found[1].peer_id()), this->state.getSelfID());

      QList<google::protobuf::RepeatedPtrField<Protos::Common::Entries>> browsed;
      const auto record = [&](const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& entries) { browsed << entries; };
      const QSharedPointer<RCC::IBrowseResult> roots = connection->browse(this->bob);
      connect(roots.data(), &RCC::IBrowseResult::result, this, record);
      roots->start();
      QTRY_COMPARE(browsed.size(), 1);
      QCOMPARE(browsed[0].size(), 1);
      QCOMPARE(browsed[0].Get(0).entries_size(), 2);

      const QSharedPointer<RCC::IBrowseResult> files = connection->browse(this->bob, browsed[0].Get(0).entries(1));
      connect(files.data(), &RCC::IBrowseResult::result, this, record);
      files->start();
      QTRY_COMPARE(browsed.size(), 2);
      QCOMPARE(browsed[1].Get(0).entries_size(), 4);

      // The GUI waits for the result of a chat message.
      int nbChatResults = 0;
      const QSharedPointer<RCC::ISendChatMessageResult> chat = connection->sendChatMessage("Hello");
      connect(chat.data(), &RCC::ISendChatMessageResult::result, this, [&](const Protos::GUI::ChatMessageResult& result) {
         QCOMPARE(result.status(), Protos::GUI::ChatMessageResult::OK);
         nbChatResults++;
      });
      chat->start();
      QTRY_COMPARE(nbChatResults, 1);

      // The commands don't change the state.
      connection->cancelDownloads({ 1, 2, 3 }, true);
      connection->pauseDownloads({ 1 });
      const int nbStates = states.size();
      QTRY_VERIFY_WITH_TIMEOUT(states.size() >= nbStates + 2, 10000);
      QCOMPARE(states.last().SerializeAsString(), this->state.getState().SerializeAsString());

      // A closed connection is forgotten and the GUI can come back.
      QCOMPARE(server.findChildren<Common::MessageSocket*>().size(), 1);
      connection->disconnectFromCore();
      QTRY_VERIFY(server.findChildren<Common::MessageSocket*>().isEmpty());
      connection->connectToCore("127.0.0.1", server.getPort(), Common::SaltedPassword());
      QTRY_VERIFY(connection->isConnected());
      QCOMPARE(connection->getRemoteID(), this->state.getSelfID());
   }
};

QTEST_GUILESS_MAIN(Tests)
#include "Tests.moc"
