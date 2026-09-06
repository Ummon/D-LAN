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
};
