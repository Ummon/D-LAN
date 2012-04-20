/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
   void iterateBreathFirst();
   void iterateDepthFirst();
   void removeElements();

private:
   static void testElementsAgainstList(const QList<int>& expected, IntTree* tree, bool withRoot);

private:
   IntTree tree;
};

#endif
