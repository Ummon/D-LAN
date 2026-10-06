#pragma once

#include <QByteArray>
#include <QHash>
#include <QList>
#include <QString>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

namespace DummyCore
{
   /**
     * What the dummy Core gives to the GUI. It's read once from a JSON file and never changes.
     *
     * The JSON document is an object, see 'DummyCore.example.json':
     *  - "peers": The known peers, exactly one of them is the dummy Core itself. For each peer:
     *     - "name": Its nick, used to refer to the peer from "search", "downloads" and "uploads".
     *     - "self": 'true' for the dummy Core.
     *     - "sharing": [size] The sharing amount. By default the size of its shared folders.
     *     - "download_rate", "upload_rate": [size] Per second, a "/s" suffix is accepted. The ones of the dummy Core are
     *       also the rates shown by the status bar.
     *     - "version": The Core version. By default the current one.
     *     - "shared": The shared folders: objects with a "name" and its content as "children".
     *       An entry with "children" is a directory, otherwise it's a file with a "size".
     *  - "search": The result of any search, each item refers to an entry of a peer:
     *     - "peer"
     *     - "path": For example "<shared folder>/<directory>/<file>".
     *     - "level": The relevance, 0 (default) is the best one.
     *  - "downloads": The queue, only files:
     *     - "path": The directory of the file, for example "Music/Album". It's the tree shown by the folders view.
     *     - "name", "size"
     *     - "status": A name from 'Protos.Common.DownloadStatus' ("common.proto"), "QUEUED" by default.
     *     - "progress": In percent, 100 by default when the status is "COMPLETE" otherwise 0.
     *     - "peer": The source.
     *     - "other_peers": The other peers which own the file.
     *  - "uploads":
     *     - "path", "name", "size": As for "downloads".
     *     - "progress": In percent.
     *     - "peer": The one which downloads the file.
     *
     * [size]: A number of bytes or a string as the GUI shows it: "670 B", "198.0 KiB", "1.5 GiB".
     */
   class State
   {
   public:
      /**
        * @return An error message, empty if OK.
        */
      QString load(const QString& filepath);
      QString loadFromJson(const QByteArray& json);

      Common::Hash getSelfID() const;

      const Protos::GUI::State& getState() const;

      /**
        * One result per peer, their tags aren't set.
        */
      const QList<Protos::Common::FindResult>& getSearchResults() const;

      Protos::GUI::BrowseResult browse(const Protos::GUI::Browse& browse) const;

   private:
      struct Files
      {
         Protos::Common::Entries roots; // The shared folders.
         QHash<QByteArray, Protos::Common::Entries> directories; // The content of each directory, see 'directoryKey(..)'.
      };

      class Loader;

      Common::Hash selfID;
      Protos::GUI::State state;
      QList<Protos::Common::FindResult> searchResults;
      QHash<Common::Hash, Files> files; // The key is the peer ID.
   };
}
