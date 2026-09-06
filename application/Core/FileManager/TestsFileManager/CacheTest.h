#pragma once

#include <QObject>

class CacheTest : public QObject
{
   Q_OBJECT
public:
   explicit CacheTest(QObject* parent = nullptr);

private slots:
   void directoryCleanupAllowsCompletion_data();
   void directoryCleanupAllowsCompletion();
   void directoryDestructionReleasesParentLocks();
   void directoryTraversalDefersDeletion();
   void retainedChunksAreDetached_data();
   void retainedChunksAreDetached();
   void chunkAccessExcludesRetirement_data();
   void chunkAccessExcludesRetirement();
   void concurrentChunkMetadata();
   void openingHandlesExcludesCompletion_data();
   void openingHandlesExcludesCompletion();
   void browseDirectoryLifetime_data();
   void browseDirectoryLifetime();
   void partialWrites_data();
   void partialWrites();
   void unfinishedFilesStayOutOfSearch_data();
   void unfinishedFilesStayOutOfSearch();
};
