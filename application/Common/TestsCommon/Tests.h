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
  
#ifndef TESTS_COMMON_H
#define TESTS_COMMON_H

#include <QTest>

#include <Hash.h>

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();

   // Global class.
   void getVersion();
   void commonPrefix();
   void nCombinations();
   void formatByteSize();
   void formatTime();
   void availableDiskSpace();
   void splitInWords();
   void hashStringToInt();

   // SortedList class.
   void sortedList();

   // IndexedArray class.
   void indexedArray();

   // TransferRateCalculator
   void transferRateCalculator();

   // PersistentData class.
   void writePersistentData();
   void readPersistentData();
   void removePersistentData();

   // Settings class.
   void writeSettings();
   void readSettings();
   void removeSettings();

   // Hash class.
   void generateAHash();
   void buildAnHashFromAString();
   void compareTwoHash();
   void hashMoveConstuctorAndAssignment();
   void hasher();

   // BloomFilter class.
   void bloomFilter();

   void messageHeader();

   // ZeroCopyOutputStreamQIODevice and ZeroCopyInputStreamQIODevice classes.
   void readAndWriteWithZeroCopyStreamQIODevice();

   // ProtoHelper
   void protoHelper();

private:
   Common::Hash hash;
};

#endif
