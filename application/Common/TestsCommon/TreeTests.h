#ifndef TREETESTS_COMMON_H
#define TREETESTS_COMMON_H

#include <QTest>

#include <Tree.h>

class IntTree : public Common::Tree<int, IntTree>
{
public:
   IntTree() {}
   IntTree(int v, IntTree* parent) : Common::Tree<int, IntTree>(v, parent) {}
};

class TreeTests : public QObject
{
   Q_OBJECT
public:
   TreeTests();

private slots:
   void initTestCase();
   void insertElements();
   void retrieveElements();
   void iterate();
   void removeElements();

private:
   static void testElementsAgainstList(const QList<int>& expected, IntTree* tree, bool withRoot);

private:
   IntTree tree;
};

#endif
