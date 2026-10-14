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
#include <QStringList>
#include <QTemporaryDir>

namespace FM
{
   class FileManager;
}

/**
  * Measures 'FM::FileManager::find(..)' on a generated set of files and directories: the searches by words,
  * by extensions, by sizes and by category, as another peer asks them.
  * Run it before and after a change and compare the results, each one is given as a 'QINFO' message.
  * It isn't part of the tests, run it with "TestsFileManager -find-benchmark" built in release.
  * The arguments of QtTest can be given too, for instance "-o results.txt,txt" or "find:common word".
  */
class FindBenchmark : public QObject
{
   Q_OBJECT
public:
   FindBenchmark();

private slots:
   void initTestCase();

   void find_data();
   void find();

   void cleanupTestCase();

private:
   QTemporaryDir sharedDirectory; ///< It stays empty: the entries are only created in the cache.
   FM::FileManager* fileManager;
   QStringList vocabulary; ///< The words the names are made of, from the most frequent to the least one.
};
