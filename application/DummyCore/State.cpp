#include <State.h>
using namespace DummyCore;

#include <algorithm>
#include <cmath>
#include <limits>

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QRegularExpression>
#include <QStringList>

#include <Common/Global.h>

namespace
{
   // As 'Common::Global::formatByteSize(..)' writes them, each one is 1024 times the previous one.
   const QStringList SIZE_UNITS { "B", "KiB", "MiB", "GiB", "TiB", "PiB" };

   /**
     * A directory is identified by what the GUI gives back when it asks for its content.
     */
   QByteArray directoryKey(const Protos::Common::Entry& directory)
   {
      return QByteArray::fromStdString(directory.shared_entry().id().hash() + directory.path() + directory.name());
   }

   void setHash(Protos::Common::Hash* hash, const Common::Hash& value)
   {
      hash->set_hash(value.getData(), Common::Hash::HASH_SIZE);
   }

   /**
     * @param value A number of bytes or a string like "1.5 GiB".
     * @exception QString
     */
   quint64 parseSize(const QJsonValue& value, const QString& location)
   {
      double size = -1;
      if (value.isDouble())
         size = value.toDouble();
      else if (value.isString())
      {
         static const QRegularExpression format("^\\s*([0-9]+(?:\\.[0-9]+)?)\\s*([A-Za-z]*)\\s*$");
         const QRegularExpressionMatch match = format.match(value.toString());
         const int unit = match.captured(2).isEmpty() ? 0 : SIZE_UNITS.indexOf(match.captured(2), 0, Qt::CaseInsensitive);
         if (match.hasMatch() && unit != -1)
            size = match.captured(1).toDouble() * std::pow(1024.0, unit);
      }

      // The GUI holds the sizes in a 'qint64'.
      if (!(size >= 0 && size < 9223372036854775808.0))
         throw QString("'%1' must be a number of bytes or a string like \"1.5 GiB\", the units are: %2").arg(location, SIZE_UNITS.join(", "));

      return static_cast<quint64>(std::round(size));
   }

   /**
     * A JSON object and where it is in the document, for the error messages. For example "peers[1].shared[0]".
     * A 'QString' is thrown when a value isn't the expected one.
     */
   class Object
   {
   public:
      Object(const QJsonValue& value, const QString& where, const QStringList& knownKeys) :
         where(where)
      {
         if (!value.isObject())
            throw QString("'%1' must be an object").arg(where);

         this->object = value.toObject();
         for (auto i = this->object.constBegin(); i != this->object.constEnd(); ++i)
            if (!knownKeys.contains(i.key()))
               throw QString("Unknown key '%1', the known ones are: %2").arg(this->location(i.key()), knownKeys.join(", "));
      }

      QString location(const QString& key) const
      {
         return this->where.isEmpty() ? key : QString(this->where % '.' % key);
      }

      bool has(const QString& key) const
      {
         return this->object.contains(key);
      }

      QString string(const QString& key, const QString& defaultValue = QString()) const
      {
         const QJsonValue value = this->object.value(key);
         if (value.isUndefined())
            return defaultValue;
         if (!value.isString())
            throw QString("'%1' must be a string").arg(this->location(key));
         return value.toString();
      }

      QString nonEmptyString(const QString& key) const
      {
         const QString value = this->string(key);
         if (value.isEmpty())
            throw QString("'%1' must be defined").arg(this->location(key));
         return value;
      }

      /**
        * The name of a file or a directory.
        */
      QString entryName() const
      {
         const QString name = this->nonEmptyString("name");
         if (name.contains('/'))
            throw QString("'%1' can't contain a '/': '%2'").arg(this->location("name"), name);
         return name;
      }

      bool boolean(const QString& key) const
      {
         const QJsonValue value = this->object.value(key);
         if (value.isUndefined())
            return false;
         if (!value.isBool())
            throw QString("'%1' must be true or false").arg(this->location(key));
         return value.toBool();
      }

      quint32 integer(const QString& key, quint32 max) const
      {
         const QJsonValue value = this->object.value(key);
         if (value.isUndefined())
            return 0;
         if (!value.isDouble() || value.toDouble() < 0 || value.toDouble() > max || value.toDouble() != std::floor(value.toDouble()))
            throw QString("'%1' must be an integer from 0 to %2").arg(this->location(key)).arg(max);
         return static_cast<quint32>(value.toDouble());
      }

      quint64 size(const QString& key, quint64 defaultValue = 0) const
      {
         const QJsonValue value = this->object.value(key);
         return value.isUndefined() ? defaultValue : parseSize(value, this->location(key));
      }

      /**
        * A size per second, 0 by default.
        */
      quint32 rate(const QString& key) const
      {
         QJsonValue value = this->object.value(key);
         if (value.isUndefined())
            return 0;

         if (value.isString() && value.toString().trimmed().endsWith("/s"))
            value = value.toString().trimmed().chopped(2);

         const quint64 rate = parseSize(value, this->location(key));
         if (rate > std::numeric_limits<quint32>::max())
            throw QString("'%1' is too high, the maximum is %2/s").arg(this->location(key), Common::Global::formatByteSize(std::numeric_limits<quint32>::max()));
         return static_cast<quint32>(rate);
      }

      /**
        * @param defaultValue In percent.
        * @return From 0 to 10000 as the progress of 'Protos.GUI.State.Upload'.
        */
      quint32 progress(int defaultValue = 0) const
      {
         const QString key("progress");
         const QJsonValue value = this->object.value(key);
         if (value.isUndefined())
            return 100 * defaultValue;
         if (!value.isDouble() || value.toDouble() < 0 || value.toDouble() > 100)
            throw QString("'%1' must be a number from 0 to 100").arg(this->location(key));
         return static_cast<quint32>(std::round(100 * value.toDouble()));
      }

      /**
        * An empty array by default.
        */
      QJsonArray array(const QString& key) const
      {
         const QJsonValue value = this->object.value(key);
         if (value.isUndefined())
            return QJsonArray();
         if (!value.isArray())
            throw QString("'%1' must be an array").arg(this->location(key));
         return value.toArray();
      }

   private:
      QJsonObject object;
      QString where;
   };
}

/**
  * @class DummyCore::State::Loader
  *
  * Fills a state from a JSON document. A 'QString' describing the first error is thrown.
  */
class State::Loader
{
public:
   Loader(State& state) :
      state(state)
   {
   }

   void load(const QJsonValue& document)
   {
      const Object object(document, QString(), { "peers", "search", "downloads", "uploads" });
      this->readPeers(object);
      this->readSearch(object);
      this->readDownloads(object);
      this->readUploads(object);
   }

private:
   struct Peer
   {
      QString name;
      Common::Hash ID;
      QHash<QString, Protos::Common::Entry> entries; // Its files and directories by their path as "search" refers to them.
   };

   void readPeers(const Object& document)
   {
      QList<Protos::GUI::State::Peer> protoPeers;
      int self = -1;

      const QJsonArray peers = document.array("peers");
      for (int i = 0; i < peers.size(); i++)
      {
         const Object object(peers[i], QString("peers[%1]").arg(i), { "name", "self", "sharing", "download_rate", "upload_rate", "version", "shared" });

         // The IDs are derived from the names: a GUI still knows the peers and their shared folders when the dummy Core is restarted.
         Peer peer { object.nonEmptyString("name"), Common::Hash(), {} };
         peer.ID = Common::Hasher::hash(peer.name);
         for (const Peer& other : std::as_const(this->peers))
            if (other.name == peer.name)
               throw QString("'%1': there is already a peer named '%2'").arg(object.location("name"), peer.name);

         if (object.boolean("self"))
         {
            if (self != -1)
               throw QString("'%1': only one peer can be the dummy Core itself").arg(object.location("self"));
            self = i;
         }

         Files files;
         quint64 sharedSize = 0;
         const QJsonArray sharedFolders = object.array("shared");
         for (int j = 0; j < sharedFolders.size(); j++)
         {
            const Object folder(sharedFolders[j], object.location(QString("shared[%1]").arg(j)), { "name", "children" });
            const QString name = folder.entryName();
            for (auto k = peer.entries.constBegin(); k != peer.entries.constEnd(); ++k)
               if (k.key().compare(name, Qt::CaseInsensitive) == 0)
                  throw QString("'%1': there is already a shared folder named '%2'").arg(folder.location("name"), name);

            // "A shared directory [..] has an empty path" and "an empty name", see 'Protos.Common.Entry'.
            Protos::Common::Entry root;
            root.set_type(Protos::Common::Entry::DIR);
            setHash(root.mutable_shared_entry()->mutable_id(), Common::Hasher::hash(QString(peer.name % '/' % name)));
            root.mutable_shared_entry()->set_shared_name(name.toStdString());
            this->readDirectory(folder, root, name, files, peer);

            peer.entries.insert(name, root);
            sharedSize += root.size();
            files.roots.add_entries()->Swap(&root);
         }

         Protos::GUI::State::Peer protoPeer;
         setHash(protoPeer.mutable_peer_id(), peer.ID);
         protoPeer.set_nick(peer.name.toStdString());
         protoPeer.set_sharing_amount(object.size("sharing", sharedSize));
         protoPeer.set_download_rate(object.rate("download_rate"));
         protoPeer.set_upload_rate(object.rate("upload_rate"));
         protoPeer.set_core_version(object.string("version", Common::Global::getVersionFull()).toStdString());
         protoPeers << protoPeer;

         this->state.files.insert(peer.ID, files);
         this->peers << peer;
      }

      if (self == -1)
         throw QString("One peer of 'peers' must be the dummy Core itself: \"self\": true");

      // "The first peer is always ourself", see 'Protos.GUI.State'.
      this->state.selfID = this->peers[self].ID;
      this->state.state.add_peers()->CopyFrom(protoPeers[self]);
      for (int i = 0; i < protoPeers.size(); i++)
         if (i != self)
            this->state.state.add_peers()->CopyFrom(protoPeers[i]);

      Protos::GUI::State::Stats* stats = this->state.state.mutable_stats();
      stats->set_cache_status(Protos::GUI::State::Stats::UP_TO_DATE);
      stats->set_download_rate(protoPeers[self].download_rate());
      stats->set_upload_rate(protoPeers[self].upload_rate());
   }

   /**
     * Reads the content ("children") of 'directory' and of all its sub-directories. Its size and 'is_empty' are set.
     * @param object The JSON object of 'directory'.
     * @param path The path of 'directory' as "search" refers to it.
     */
   void readDirectory(const Object& object, Protos::Common::Entry& directory, const QString& path, Files& files, Peer& peer)
   {
      struct Child
      {
         QString name;
         Protos::Common::Entry entry;
      };
      QList<Child> children;

      // The path is relative to the shared folder and "doesn't contain the entry name", see 'Protos.Common.Entry'.
      // Only a shared folder has an empty name.
      const std::string childrenPath = directory.name().empty() ? std::string() : directory.path() + directory.name() + '/';

      const QJsonArray array = object.array("children");
      for (int i = 0; i < array.size(); i++)
      {
         const Object childObject(array[i], object.location(QString("children[%1]").arg(i)), { "name", "size", "children" });

         Child child { childObject.entryName(), Protos::Common::Entry() };
         for (const Child& other : std::as_const(children))
            if (other.name.compare(child.name, Qt::CaseInsensitive) == 0)
               throw QString("'%1': there is already an entry named '%2' in this directory").arg(childObject.location("name"), child.name);

         child.entry.set_path(childrenPath);
         child.entry.set_name(child.name.toStdString());
         child.entry.mutable_shared_entry()->CopyFrom(directory.shared_entry());

         const QString childPath = path % '/' % child.name;
         if (childObject.has("children"))
         {
            if (childObject.has("size"))
               throw QString("'%1': the size of a directory is the one of its content, it can't be given").arg(childObject.location("size"));
            child.entry.set_type(Protos::Common::Entry::DIR);
            this->readDirectory(childObject, child.entry, childPath, files, peer);
         }
         else
         {
            child.entry.set_type(Protos::Common::Entry::FILE);
            child.entry.set_size(childObject.size("size"));
         }

         peer.entries.insert(childPath, child.entry);
         children << child;
      }

      // As the Core does: the directories first, the names are sorted without regard to case.
      std::sort(children.begin(), children.end(), [](const Child& c1, const Child& c2) {
         if (c1.entry.type() != c2.entry.type())
            return c1.entry.type() == Protos::Common::Entry::DIR;
         return c1.name.compare(c2.name, Qt::CaseInsensitive) < 0;
      });

      quint64 size = 0;
      Protos::Common::Entries entries;
      for (Child& child : children)
      {
         size += child.entry.size();
         entries.add_entries()->Swap(&child.entry);
      }

      directory.set_size(size);
      directory.set_is_empty(children.isEmpty());
      files.directories.insert(directoryKey(directory), entries);
   }

   void readSearch(const Object& document)
   {
      QHash<Common::Hash, int> resultIndices; // A 'Protos.Common.FindResult' holds the entries of only one peer.

      const QJsonArray search = document.array("search");
      for (int i = 0; i < search.size(); i++)
      {
         const Object object(search[i], QString("search[%1]").arg(i), { "peer", "path", "level" });

         const Peer& peer = this->peer(object, "peer");
         const QString path = object.string("path").split('/', Qt::SkipEmptyParts).join('/');
         const auto entry = peer.entries.constFind(path);
         if (entry == peer.entries.constEnd())
            throw QString("'%1': '%2' isn't shared by the peer '%3'").arg(object.location("path"), path, peer.name);

         auto index = resultIndices.constFind(peer.ID);
         if (index == resultIndices.constEnd())
         {
            index = resultIndices.insert(peer.ID, this->state.searchResults.size());
            Protos::Common::FindResult result;
            setHash(result.mutable_peer_id(), peer.ID);
            this->state.searchResults << result;
         }

         Protos::Common::FindResult::EntryLevel* entryLevel = this->state.searchResults[*index].add_entries();
         entryLevel->set_level(object.integer("level", 100));
         entryLevel->mutable_entry()->CopyFrom(*entry);
      }
   }

   void readDownloads(const Object& document)
   {
      const QJsonArray downloads = document.array("downloads");
      for (int i = 0; i < downloads.size(); i++)
      {
         const Object object(downloads[i], QString("downloads[%1]").arg(i), { "path", "name", "size", "status", "progress", "peer", "other_peers" });

         Protos::GUI::State::Download* download = this->state.state.add_downloads();
         download->set_id(i + 1); // "Cannot be 0".
         Protos::Common::Entry* entry = download->mutable_local_entry();
         readFile(object, entry);

         Protos::Common::DownloadStatus status = Protos::Common::QUEUED;
         const QString statusName = object.string("status", "QUEUED");
         if (!Protos::Common::DownloadStatus_Parse(statusName.toUpper().toStdString(), &status))
            throw QString("'%1': unknown status '%2', see 'Protos.Common.DownloadStatus'").arg(object.location("status"), statusName);
         download->set_status(status);

         // The GUI shows '10000 * downloaded_bytes / size' rounded down: it's the smallest number of bytes giving the asked progress.
         const quint32 progress = object.progress(status == Protos::Common::COMPLETE ? 100 : 0);
         download->set_downloaded_bytes(entry->size() / 10000 * progress + (entry->size() % 10000 * progress + 9999) / 10000);
         entry->set_exists(download->downloaded_bytes() > 0 || status == Protos::Common::COMPLETE);

         // "The first one always corresponds to the peer source".
         const Peer& source = this->peer(object, "peer");
         setHash(download->add_peer_ids(), source.ID);
         download->set_peer_source_nick(source.name.toStdString());

         const QJsonArray otherPeers = object.array("other_peers");
         for (int j = 0; j < otherPeers.size(); j++)
            setHash(download->add_peer_ids(), this->peer(otherPeers[j], object.location(QString("other_peers[%1]").arg(j))).ID);
      }
   }

   void readUploads(const Object& document)
   {
      const QJsonArray uploads = document.array("uploads");
      for (int i = 0; i < uploads.size(); i++)
      {
         const Object object(uploads[i], QString("uploads[%1]").arg(i), { "path", "name", "size", "progress", "peer" });

         Protos::GUI::State::Upload* upload = this->state.state.add_uploads();
         upload->set_id(i + 1);
         readFile(object, upload->mutable_file());
         upload->set_progress(object.progress());
         setHash(upload->mutable_peer_id(), this->peer(object, "peer").ID);
      }
   }

   /**
     * Reads a file of "downloads" or "uploads".
     */
   static void readFile(const Object& object, Protos::Common::Entry* file)
   {
      const QStringList directories = object.string("path").split('/', Qt::SkipEmptyParts);

      file->set_type(Protos::Common::Entry::FILE);
      if (!directories.isEmpty())
         file->set_path(QString(directories.join('/') % '/').toStdString());
      file->set_name(object.entryName().toStdString());
      file->set_size(object.size("size"));
   }

   const Peer& peer(const Object& object, const QString& key) const
   {
      return this->peer(object.string(key), object.location(key));
   }

   const Peer& peer(const QJsonValue& name, const QString& location) const
   {
      for (const Peer& peer : this->peers)
         if (name.isString() && peer.name == name.toString())
            return peer;

      throw QString("'%1' must be the name of a peer of 'peers'").arg(location);
   }

   State& state;
   QList<Peer> peers;
};

/**
  * @class DummyCore::State
  */

QString State::load(const QString& filepath)
{
   QFile file(filepath);
   if (!file.open(QIODevice::ReadOnly))
      return QString("Unable to open the state file '%1': %2").arg(filepath, file.errorString());

   const QString error = this->loadFromJson(file.readAll());
   return error.isEmpty() ? QString() : QString("Invalid state file '%1': %2").arg(filepath, error);
}

QString State::loadFromJson(const QByteArray& json)
{
   *this = State();

   QJsonParseError parseError;
   const QJsonDocument document = QJsonDocument::fromJson(json, &parseError);
   if (parseError.error != QJsonParseError::NoError)
      return QString("%1 (line %2)").arg(parseError.errorString()).arg(json.left(parseError.offset).count('\n') + 1);
   if (!document.isObject())
      return "The document must be a JSON object";

   try
   {
      Loader(*this).load(document.object());
   }
   catch (const QString& error)
   {
      *this = State();
      return error;
   }

   return QString();
}

Common::Hash State::getSelfID() const
{
   return this->selfID;
}

const Protos::GUI::State& State::getState() const
{
   return this->state;
}

const QList<Protos::Common::FindResult>& State::getSearchResults() const
{
   return this->searchResults;
}

/**
  * Answers as the Core does: the content of each asked directory then, if asked, the shared folders.
  * Nothing is known about an unknown peer.
  */
Protos::GUI::BrowseResult State::browse(const Protos::GUI::Browse& browse) const
{
   Protos::GUI::BrowseResult result;
   result.set_tag(browse.tag());

   const auto peerFiles = this->files.constFind(Common::Hash(browse.peer_id().hash()));
   if (peerFiles == this->files.constEnd())
      return result;

   for (const Protos::Common::Entry& directory : browse.dirs().entries())
   {
      Protos::Common::Entries* entries = result.add_entries();
      const auto content = peerFiles->directories.constFind(directoryKey(directory));
      if (content != peerFiles->directories.constEnd())
         entries->CopyFrom(*content);
   }

   if (browse.dirs().entries_size() == 0 || browse.get_roots())
      result.add_entries()->CopyFrom(peerFiles->roots);

   return result;
}
