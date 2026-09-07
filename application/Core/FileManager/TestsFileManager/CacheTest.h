#pragma once

#include <QObject>

class CacheTest : public QObject
{
   Q_OBJECT
public:
   explicit CacheTest(QObject* parent = nullptr);

private slots:
   void watcherRecovery_data();
   void watcherRecovery();
   void updaterWatcherRecovery_data();
   void updaterWatcherRecovery();
   void watchedFileRename_data();
   void watchedFileRename();
   void updaterWaitsForEarliestTask_data();
   void updaterWaitsForEarliestTask();
   void failedHashingIsQueuedOnce_data();
   void failedHashingIsQueuedOnce();
   void hashingSchedulerTransitions();
   void hashingWorkFollowsFileChanges();
   void directoryTotalsFollowFileResizing();
   void fittestDirectoryMatchesExistingPaths();
   void newDirectoryPreservesFinalComponent_data();
   void newDirectoryPreservesFinalComponent();
   void directoryMovesAllowCompletion_data();
   void directoryMovesAllowCompletion();
   void directoryLookupAllowsSizePropagation_data();
   void directoryLookupAllowsSizePropagation();
   void directoryCreationDefersDeletion();
   void hashingInvalidatesChangedFiles_data();
   void hashingInvalidatesChangedFiles();
   void redownloadStopsActiveHashing();
   void deferredHashPersistence_data();
   void deferredHashPersistence();
   void hashResultsOnlySendOutstandingChunks();
   void hashRequestsKeepOriginalGeneration_data();
   void hashRequestsKeepOriginalGeneration();
   void writerRegistrationSurvivesFileReset();
   void hashingRespectsChunkBoundaries_data();
   void hashingRespectsChunkBoundaries();
   void handlesReopenAfterCompletion_data();
   void handlesReopenAfterCompletion();
   void readerReopenFailureAfterCompletion();
   void directoryCleanupAllowsCompletion_data();
   void directoryCleanupAllowsCompletion();
   void directoryDestructionReleasesParentLocks();
   void directoryTraversalDefersDeletion();
   void retainedChunksAreDetached_data();
   void retainedChunksAreDetached();
   void chunkAccessExcludesRetirement_data();
   void chunkAccessExcludesRetirement();
   void concurrentChunkMetadata();
   void metadataReadersAvoidStructuralLocks();
   void concurrentEntryMetadata();
   void invalidDownloadEntries_data();
   void invalidDownloadEntries();
   void downloadEntryPathsAndHashes_data();
   void downloadEntryPathsAndHashes();
   void openingHandlesExcludesCompletion_data();
   void openingHandlesExcludesCompletion();
   void browseDirectoryLifetime_data();
   void browseDirectoryLifetime();
   void partialWrites_data();
   void partialWrites();
   void unfinishedDownloadRetry_data();
   void unfinishedDownloadRetry();
   void emptyFileReplacement_data();
   void entryTypeReplacement_data();
   void entryTypeReplacement();
   void emptyFileReplacement();
   void emptyFileReplacementReportsRenameFailure();
   void unfinishedFilesStayOutOfSearch_data();
   void unfinishedFilesStayOutOfSearch();
   void sharedFileRenameUpdatesSearchIndexes();
};
