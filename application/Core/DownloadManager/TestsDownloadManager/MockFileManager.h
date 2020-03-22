#ifndef TESTS_DOWNLOADMANAGER_MOCKFILEMANAGER_H
#define TESTS_DOWNLOADMANAGER_MOCKFILEMANAGER_H

#include <FileManager/IFileManager.h>

class MockFileManager : public FM::IFileManager
{
   Q_OBJECT
public:
   MockFileManager();
   ~MockFileManager();

   void setSharedDirs(const QStringList& dirs);
   QPair<Common::SharedDir, QString> addASharedDir(const QString& absoluteDir);
   QList<Common::SharedDir> getSharedDirs() const;
   QString getSharedDir(const Common::Hash& ID) const;
   QSharedPointer<FM::IChunk> getChunk(const Common::Hash& hash) const;
   QList<QSharedPointer<FM::IChunk>> getAllChunks(const Protos::Common::Entry& localEntry, const Common::Hashes& hashes) const;
   QList<QSharedPointer<FM::IChunk>> newFile(Protos::Common::Entry& entry);
   void newDirectory(Protos::Common::Entry& entry);
   QSharedPointer<FM::IGetHashesResult> getHashes(const Protos::Common::Entry& file);
   QSharedPointer<FM::IGetEntriesResult> getScannedEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry = std::numeric_limits<int>::max());
   Protos::Common::Entries getEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry = std::numeric_limits<int>::max());
   Protos::Common::Entries getEntries();
   QList<Protos::Common::FindResult> find(const QString& words, int maxNbResult, int maxSize);
   QList<Protos::Common::FindResult> find(const QString& words, const QList<QString>& extensions, qint64 minFileSize, qint64 maxFileSize, Protos::Common::FindPattern_Category category, int maxNbResult, int maxSize);
   QBitArray haveChunks(const QList<Common::Hash>& hashes);
   quint64 getAmount();
   CacheStatus getCacheStatus() const;
   int getProgress() const;
   void dumpWordIndex() const;
   void printSimilarFiles() const;
};

#endif
