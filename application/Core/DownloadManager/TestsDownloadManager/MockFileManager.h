#ifndef TESTS_DOWNLOADMANAGER_MOCKFILEMANAGER_H
#define TESTS_DOWNLOADMANAGER_MOCKFILEMANAGER_H

#include <FileManager/IFileManager.h>

class MockFileManager : public FM::IFileManager
{
   Q_OBJECT
public:
   MockFileManager();
   ~MockFileManager();

   void setSharedPaths(const QList<SharedPath>& paths) override;
   QPair<Common::SharedEntry, QString> addASharedPath(const QString& absoluteDir) override;
   QList<Common::SharedEntry> getSharedEntries() const override;
   QString getSharedEntry(const Common::Hash& ID) const override;
   QSharedPointer<FM::IChunk> getChunk(const Common::Hash& hash) const override;
   QList<QSharedPointer<FM::IChunk>> getAllChunks(const Protos::Common::Entry& localEntry, const QList<Common::Hash>& hashes) const override;
   void updateFromQueueEntry(const Protos::Queue::Queue_Entry& entry) override;
   QList<QSharedPointer<FM::IChunk>> newFile(Protos::Common::Entry& entry) override;
   void newDirectory(Protos::Common::Entry& entry) override;
   QSharedPointer<FM::IGetHashesResult> getHashes(const Protos::Common::Entry& file) override;
   QSharedPointer<FM::IGetEntriesResult> getScannedEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry = std::numeric_limits<int>::max()) override;
   Protos::Common::Entries getEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry = std::numeric_limits<int>::max()) override;
   Protos::Common::Entries getEntries() override;
   QList<Protos::Common::FindResult> find(const QString& words, int maxNbResult, int maxSize) override;
   QList<Protos::Common::FindResult> find(const QString& words, const QList<QString>& extensions, qint64 minFileSize, qint64 maxFileSize, Protos::Common::FindPattern_Category category, int maxNbResult, int maxSize, bool setSharedEntryPath) override;
   QBitArray haveChunks(const QList<Common::Hash>& hashes) override;
   qint64 getAmount() override;
   CacheStatus getCacheStatus() const override;
   int getProgress() const override;
   QString getWordIndex_debug() const override;
   QString getSimilarFiles_debug() const override;
   QString getCacheTree_debug() const override;
};

#endif
