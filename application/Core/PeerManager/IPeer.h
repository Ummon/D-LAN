#ifndef PEERMANAGER_IPEER_H
#define PEERMANAGER_IPEER_H

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>
#include <QHostAddress>

#include <Common/Hashes.h>

namespace PM
{
   class IGetEntries;
   class IGetHashes;

   class IPeer : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IPeer() {}

      virtual Common::Hash getID() = 0;
      virtual QHostAddress getIP() = 0;
      virtual QString getNick() = 0;
      virtual quint64 getSharingAmount() = 0;

      virtual bool isAlive() = 0;

      //virtual bool send(const QByteArray& data) = 0;

      /**
        * Ask for the entries in a given directory.
        * This method is non-blocking, the entries will be delivered by the signal
        * 'entriesResult'.
        * If a second getEntries
        */
      virtual void getEntries(const Protos::Common::Entry& dir) = 0;

      /**
        * Ask for the hashes of a given file.
        * This method is non-blocking, the hashes will be delivered by the signal 'nextHashResult' followed by
        * one or more 'nextHash' signal.
        */
      virtual void getHashes(const Protos::Common::FileEntry& file) = 0;

      /**
        * Ask to download a chunk.
        */
      virtual void getChunk(const Protos::Core::GetChunk& chunk) = 0;

   signals:
      void entriesResult(const Protos::Core::GetEntriesResult& entries);

      void nextHashResult(Protos::Core::GetHashesResult hashesResult);
      void nextHash(Common::Hash hash);

      void chunkData(QIODevice* device);

      //void nextHashError();

      /*
   signals:
      void receive(QByteArray& data);

   public slots:
      virtual void connected() = 0;
      virtual void gotData() = 0;*/
   };
}
#endif
