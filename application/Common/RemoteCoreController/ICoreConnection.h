/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/Network/MessageSocket.h>
#include <Common/LogManager/IEntry.h>

namespace RCC
{
   class IBrowseResult;
   class ISearchResult;

   /**
     * The main interface to control a remote core.
     * The signal 'newState' is periodically emitted, for exemple each second. It can be emitted right after certain action, like 'setCoreSettings(..)'.
     * See the prototype file "application/Protos/gui_protocol.proto" for more information.
     */
   class ICoreConnection : public Common::MessageSocket
   {
      Q_OBJECT
   public:
      virtual ~ICoreConnection() {};

      /**
        * Connect to a local core with the default port (59485).
        * When the connection is ready, the signal 'coreConnected' is emitted.
        */
      virtual void connectToCore() = 0;

      /**
        * Connect to a local core with a given port.
        * When the connection is ready, the signal 'coreConnected' is emitted.
        */
      virtual void connectToCore(quint16 port) = 0;

      /**
        * Connect to a remote core. Password is mendatory and should be hashed and salted, see the class 'Common::Hasher'.
        * When the connection is ready, the signal 'coreConnected' is emitted.
        */
      virtual void connectToCore(const QString& address, quint16 port, Common::Hash password) = 0;

      virtual void sendChatMessage(const QString& message) = 0;

      /**
        * @remarks The signal 'newState' will be emitted right after a call.
        */
      virtual void setCoreSettings(const Protos::GUI::CoreSettings settings) = 0;

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
        * Cancel one or more download. IDs are given by the signal 'newState'.
        * @remarks The signal 'newState' will be emitted right after a call.
        */
      virtual void cancelDownloads(const QList<quint64>& downloadIDs) = 0;

      /**
        * @remarks The signal 'newState' will be emitted right after a call.
        */
      virtual void moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, bool moveBefore = true) = 0;

      /**
        * Ask to emit the signal 'newState'.
        */
      virtual void refresh() = 0;

      virtual bool isRunningAsSubProcess() = 0;

   signals:
      void coreConnected();
      void coreDisconnected();

      void newState(const Protos::GUI::State&);
      void newChatMessage(const Common::Hash& peerID, const QString& message);
      void newLogMessage(QSharedPointer<const LM::IEntry> entry);
   };
}

#endif
