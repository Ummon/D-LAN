#pragma once

#include <QObject>

class CacheTest : public QObject
{
   Q_OBJECT
public:
   explicit CacheTest(QObject* parent = nullptr);

private slots:
   void retainedChunksAreDetached_data();
   void retainedChunksAreDetached();
   void openingHandlesExcludesCompletion_data();
   void openingHandlesExcludesCompletion();
   void browseDirectoryLifetime_data();
   void browseDirectoryLifetime();
   void partialWrites_data();
   void partialWrites();
};
