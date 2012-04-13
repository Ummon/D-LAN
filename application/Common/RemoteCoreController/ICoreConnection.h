/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#ifndef RCC_ICORECONNECTION_H
#define RCC_ICORECONNECTION_H

#include <QObject>
#include <QLocale>
#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/LogManager/IEntry.h>

namespace RCC
{
   class IBrowseResult;
   class ISearchResult;

   /**
     * The main interface to control a remote core.
     * The signal 'newState' is periodically emitted, for exemple each second. It can be emitted right after certain action, like 'setCoreSettings(..)'.
     * See the prototype file "application/Protos/gui_protocol.proto" for more information.
     *
     * Connection process:
     *  - Call 'connectToCore(..)'.
     *  - if error: signal 'connectingError()' is emitted.
     *  - if ok:
     *    - if previsouly connected: signal 'coreDisconnected()' is emitted.
     *    - signal 'coreConnected()' is emitted.
     */
   class ICoreConnection : public QObject
   {
      Q_OBJECT
   public:
      enum ConnectionErrorCode
      {
         ERROR_ALREADY_CONNECTED_TO_THIS_CORE = 1,
         ERROR_CONNECTING_IN_PROGRESS = 2,
         ERROR_HOST_UNKOWN = 3,
         ERROR_HOST_TIMEOUT = 4,
         ERROR_NO_REMOTE_PASSWORD_DEFINED = 5,
         ERROR_WRONG_PASSWORD = 6,
         ERROR_INVALID_ADDRESS = 7,
         ERROR_UNKNOWN = 255
      };

      virtual ~ICoreConnection() {}

      /**
        * Connect to a local core with the default port (59485).
        * When the connection is ready, the signal 'coreConnected' is emitted.
        * If the connection fail, the signal 'connectingError(..)' si emitted.
        */
      virtual void connectToCore() = 0;

      /**
        * Connect to a local core with a given port.
        */
      virtual void connectToCore(quint16 port) = 0;

      /**
        * Connect to a remote core. Password is mendatory and should be hashed and salted, see the class 'Common::Hasher'.
        * @param address the IP adress, it can be an IPv4 or IPv6 address.
        */
      virtual void connectToCore(const QString& address, quint16 port, Common::Hash password) = 0;

      /**
        * Same as the method above but takes a plain password, it will be automatically salted.
        */
      virtual void connectToCore(const QString& address, quint16 port, const QString& password) = 0;

      virtual Common::Hash getRemoteID() const = 0;

      virtual bool isLocal() const = 0;

      /**
        * If connected to the core AND authenticated.
        */
      virtual bool isConnected() const = 0;

      virtual bool isConnecting() const = 0;

      virtual void disconnectFromCore() = 0;

      virtual void sendChatMessage(const QString& message) = 0;

      /**
        * @remarks The signal 'newState' will be emitted right after a call.
        */
      virtual void setCoreSettings(const Protos::GUI::CoreSettings settings) = 0;

      /**
        * Define the core language, as soon as a connection to a core is established the language
        * is sent to it.
        */
      virtual void setCoreLanguage(const QLocale locale) = 0;

      /**
        * @param newPassword
        * @param oldPassword Hased + salted.
        * @return true if the given old password match the current password else false.
        */
      virtual bool setCorePassword(const QString& newPassword, const QString& oldPassword = QString()) = 0;

      /**
        * Don't need to provide the old password to reset a password.
        */
      virtual void resetCorePassword() = 0;

      /**
        * Get the roots folders (shared directories) of a given peer.
        * @param peerID Can be yourself.
        */
      virtual QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID) = 0;

      /**
        * Get files and folders from one folder.
        * @param peerID Can be yourself.
        * @param entry A folder from the remote peer.
        */
      virtual QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entry& entry) = 0;

      /**
        * Get files and folders from some folders. Plus the root folders if asked.
        * @param peerID Can be yourself.
        * @param entries One or more folders frome the remote peer.
        * @param withRoots
        */
      virtual QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots = true) = 0;

      /**
        * Search some files and folders to the entire network, do not search in our own folders.
        */
      virtual QSharedPointer<ISearchResult> search(const QString& terms) = 0;

      /**
        * Queue an entry to download, it can be a folder or a file.
        * @remarks The signal 'newState' will be emitted right after a call.
        */
      virtual void download(const Common::Hash& peerID, const Protos::Common::Entry& entry) = 0;

      /**
        * @param relativePath Must ended with a slash ('/').
        */
      virtual void download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const Common::Hash& sharedFolderID, const QString& relativePath = "/") = 0;

      /**
        * Download an entry 'entry' from the peer 'peerID' to a local path 'absolutePath'. If there is no shared directory corresponding
        * to the given path a new shared directory is automatically created.
        */
      virtual void download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const QString& absolutePath) = 0;

      /**
        * Cancel one or more download. IDs are given by the signal 'newState'.
        * @remarks The signal 'newState' will be emitted right after a call.
        * @param complete If true remove all complete download.
        */
      virtual void cancelDownloads(const QList<quint64>& downloadIDs, bool complete = false) = 0;

      /**
        * Pause or unpause one or more download. IDs are given by the signal 'newState'.
        * @remarks The signal 'newState' will be emitted right after a call.
        */
      virtual void pauseDownloads(const QList<quint64>& downloadIDs, bool pause = true) = 0;

      /**
        * @remarks The signal 'newState' will be emitted right after a call.
        */
      virtual void moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position = Protos::GUI::MoveDownloads::BEFORE) = 0;
      virtual void moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position = Protos::GUI::MoveDownloads::BEFORE) = 0;

      /**
        * Ask a new state from the core, the signal 'newState' is then emitted.
        */
      virtual void refresh() = 0;

      virtual bool isRunningAsSubProcess() const = 0;

      struct ConnectionInfo {
         void clear() { this->address.clear(); this->port = 0; this->password = Common::Hash(); }
         QString address;
         quint16 port;
         Common::Hash password;
      };

      virtual ConnectionInfo getConnectionInfo() const = 0;

      /**
        * These data are valid during the signal 'connecting' to the signal 'connected'.
        */
      virtual ConnectionInfo getConnectionInfoConnecting() const = 0;

   signals:
      void connecting();
      void connectingError(RCC::ICoreConnection::ConnectionErrorCode); // Can only be thrown during the connection process.
      void connected();
      void disconnected(bool asked); // Can only be thrown if 'coreConnected()' has been previously thrown. 'asked' = true if disconnected by 'disconnectFromCore()'.

      void newState(const Protos::GUI::State&);
      void newChatMessages(const Protos::GUI::EventChatMessages&);
      void newLogMessage(QSharedPointer<const LM::IEntry>);
   };
}

#endif
