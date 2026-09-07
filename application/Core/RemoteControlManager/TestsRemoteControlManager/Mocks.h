#pragma once

#include <cstring>

#include <priv/RemoteConnection.h>

// An in-memory socket makes data buffered before startListening deterministic.
class BufferedSocket : public QTcpSocket
{
public:
   QByteArray input;
   QByteArray output;

   explicit BufferedSocket(bool local = true)
   {
      this->setOpenMode(QIODevice::ReadWrite | QIODevice::Unbuffered);
      this->setSocketState(QAbstractSocket::ConnectedState);
      this->setPeerAddress(QHostAddress(local ? "127.0.0.1" : "192.0.2.1"));
   }

   qint64 bytesAvailable() const override { return QTcpSocket::bytesAvailable() + this->input.size(); }
   bool atEnd() const override { return this->bytesAvailable() == 0; }
   void close() override
   {
      if (this->state() == QAbstractSocket::UnconnectedState)
         return;
      this->setSocketState(QAbstractSocket::UnconnectedState);
      this->setOpenMode(QIODevice::NotOpen);
      emit disconnected();
   }

   void receive(Common::MessageHeader::MessageType type, const google::protobuf::Message& message)
   {
      QByteArray bytes(Common::MessageHeader::HEADER_SIZE + message.ByteSizeLong(), Qt::Uninitialized);
      Common::Message::writeMessageToBuffer(bytes.data(), bytes.size(),
         Common::MessageHeader(type, message.ByteSizeLong(), Common::Hash()), &message);
      this->input += bytes;
      emit readyRead();
   }

   QList<Common::Message> messages() const
   {
      QList<Common::Message> result;
      qsizetype offset = 0;
      while (offset < this->output.size())
      {
         auto message = Common::Message::readMessage(this->output.constData() + offset, this->output.size() - offset);
         offset += Common::MessageHeader::HEADER_SIZE + message.getHeader().getSize();
         result << message;
      }
      return result;
   }

protected:
   qint64 readData(char* data, qint64 size) override
   {
      const qint64 count = qMin(size, qint64(this->input.size()));
      std::memcpy(data, this->input.constData(), count);
      this->input.remove(0, count);
      return count;
   }
   qint64 writeData(const char* data, qint64 size) override
   {
      this->output.append(data, size);
      return size;
   }
};

class FileManager : public FM::IFileManager
{
public:
   void setSharedPaths(const QList<SharedPath>&) override {}
   QPair<Common::SharedEntry, QString> addASharedPath(const QString&) override { return {}; }
   QList<Common::SharedEntry> getSharedEntries() const override { return {}; }
   QString getSharedEntry(const Common::Hash&) const override { return {}; }
   QSharedPointer<FM::IChunk> getChunk(const Common::Hash&) const override { return {}; }
   QList<QSharedPointer<FM::IChunk>> getAllChunks(const Protos::Common::Entry&, const QList<Common::Hash>&) const override { return {}; }
   void updateFromQueueEntry(const Protos::Queue::Queue_Entry&) override {}
   QList<QSharedPointer<FM::IChunk>> newFile(Protos::Common::Entry&) override { return {}; }
   void newDirectory(Protos::Common::Entry&) override {}
   QSharedPointer<FM::IGetHashesResult> getHashes(const Protos::Common::Entry&) override { return {}; }
   QSharedPointer<FM::IGetEntriesResult> getScannedEntries(const Protos::Common::Entry&, int) override { return {}; }
   Protos::Common::Entries getEntries(const Protos::Common::Entry&, int) override { return {}; }
   Protos::Common::Entries getEntries() override { return {}; }
   QList<Protos::Common::FindResult> find(const QString&, int, int) override { return {}; }
   QList<Protos::Common::FindResult> find(const QString&, const QList<QString>&, qint64, qint64, Protos::Common::FindPattern_Category, int, int, bool) override { return {}; }
   QBitArray haveChunks(const QList<Common::Hash>&) override { return {}; }
   qint64 getAmount() override { return 0; }
   CacheStatus getCacheStatus() const override { return UP_TO_DATE; }
   int getProgress() const override { return 0; }
   QString getWordIndex_debug() const override { return {}; }
   QString getSimilarFiles_debug() const override { return {}; }
   QString getCacheTree_debug() const override { return {}; }
};

class UploadManager : public UM::IUploadManager
{
public:
   QList<UM::IChunksUploader*> getChunksUploaders() const override { return {}; }
   int getUploadRate() override { return 0; }
};

class DownloadManager : public DM::IDownloadManager
{
public:
   void addDownload(const Protos::Common::Entry&, PM::IPeer*, const Common::Hash&, const QString&) override {}
   void addDownload(const Protos::Common::Entry&, PM::IPeer*, const QString&) override {}
   QList<DM::IDownload*> getDownloads() const override { return {}; }
   void moveDownloads(const QList<quint64>&, const QList<quint64>&, Protos::GUI::MoveDownloads::Position) override {}
   void removeAllCompleteDownloads() override {}
   void removeDownloads(QList<quint64>) override {}
   void pauseDownloads(QList<quint64>, bool) override {}
   QList<QSharedPointer<DM::IChunkDownloader>> getTheFirstUnfinishedChunks(int) override { return {}; }
   QList<QSharedPointer<DM::IChunkDownloader>> getTheOldestUnfinishedChunks(int) override { return {}; }
   int getDownloadRate() override { return 0; }
};

class ChatSystem : public CS::IChatSystem
{
public:
   SendStatus send(const QString&, const QString&, const QList<Common::Hash>&) override { return SendStatus::OK; }
   void getLastChatMessages(Protos::Common::ChatMessages&, int, const QString&) const override {}
   QList<ChatRoom> getRooms() const override { return {}; }
   void joinRoom(const QString&) override {}
   void leaveRoom(const QString&) override {}
};
