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
   void nbChunks();
   void formatByteSize();
   void formatTime();
   void availableDiskSpace();
   void splitInWords();
   void hashStringToInt();

   // Path class.
   void path();

   // SortedList class.
   void sortedList();

   // SortedArray class
   void sortedArray();
   void sortedArrayCopy();
   void sortedArrayCopyException();
   void sortedArrayIndexedCopyOnWrite();
   void sortedArrayConstIterator();
   void sortedArraySubscriptCascadingSplit();
   void sortedArrayInternalNodeIndices();
   void sortedArrayInsertIndex();
   void sortedArrayToList();
   void sortedArrayComparatorCollisions();
   void sortedArrayComparatorException();
   void sortedArrayEmptyNearestIndex();
   void sortedArrayComparatorConstructor();
   void sortedArrayStandardIterator();
   void sortedArrayClearException();

   // MapArray class.
   void mapArray();

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
   void hashMoveConstructorAndAssignment();
   void hasher();
   void hasherHashValue();

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
