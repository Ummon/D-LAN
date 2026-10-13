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

#pragma once

#include <QTest>
#include <QList>
#include <QStringList>

#include <priv/WordIndex/WordIndex.h>

/**
  * Measures 'FM::WordIndex' on a generated set of file names, to compare two implementations of it:
  * run it before and after a change and compare the results, each one is given as a 'QINFO' message.
  * It isn't part of the tests, run it with "TestsFileManager -benchmark" built in release.
  * The arguments of QtTest can be given too, for instance "-o results.txt,txt" or "search:common word".
  */
class WordIndexBenchmark : public QObject
{
   Q_OBJECT
public:
   WordIndexBenchmark();

private slots:
   void initTestCase();

   void memory();
   void addItems();
   void rmItems();
   void search_data();
   void search();

private:
   void fill(FM::WordIndex<const QStringList*>& index) const;

   QStringList vocabulary; ///< The words the names are made of, from the most frequent to the least one.
   QList<QStringList> names; ///< The words of each name as they are indexed.
   FM::WordIndex<const QStringList*> index; ///< The items are the elements of 'names'.
   qint64 indexMemory; ///< [byte]. -1 if unknown.
};
