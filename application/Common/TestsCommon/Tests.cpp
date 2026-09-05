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

#include <Tests.h>

#include <algorithm>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <vector>

#include <QtDebug>
#include <QByteArray>
#include <QFile>
#include <QMap>
#include <QDir>
#include <QElapsedTimer>
#include <QRandomGenerator64>

#include <Common/LogManager/Builder.h>

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Containers/SortedList.h>
#include <Containers/SortedArray.h>
#include <Containers/MapArray.h>
#include <Network/MessageHeader.h>
#include <Constants.h>
#include <PersistentData.h>
#include <Settings.h>
#include <Global.h>
#include <Path.h>
#include <StringUtils.h>
#include <ZeroCopyStreamQIODevice.h>
#include <ProtoHelper.h>
#include <BloomFilter.h>
#include <TransferRateCalculator.h>
using namespace Common;

Tests::Tests()
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();

   QTest::qSleep(100); // If there is no delay when debugging, the debugger is not attached fast enough and some breakpoints are not triggered... very strange.
   qDebug() << "Application directory path (where the settings and persistent data are put) : " << Global::getDataFolder(Common::Global::DataFolderType::ROAMING, false);
}

void Tests::getVersion()
{
   qDebug() << "Global::getVersion(): " << Global::getVersion();
   qDebug() << "Global::getVersionTag(): " << Global::getVersionTag();
   qDebug() << "Global::getSystemVersion(): " << Global::getSystemVersion();
   qDebug() << "Global::getVersionFull(): " << Global::getVersionFull();

   QVERIFY(!Global::getVersion().isEmpty());
   QVERIFY(!Global::getVersionTag().isEmpty());
   QVERIFY(!Global::getSystemVersion().isEmpty());
   QVERIFY(!Global::getVersionFull().isEmpty());
}

void Tests::commonPrefix()
{
   const QString s1 = "abcd";
   const QString s2 = "abc";
   const QString s3 = "abcz";
   const QString s4 = "a";
   const QString s5 = "";
   const QString s6;
   const QString s7 = "abcdefg";
   const QString s8 = "zzz";

   QCOMPARE(StringUtils::commonPrefix(s1, s2), 3);
   QCOMPARE(StringUtils::commonPrefix(s1, s3), 3);
   QCOMPARE(StringUtils::commonPrefix(s1, s4), 1);
   QCOMPARE(StringUtils::commonPrefix(s1, s5), 0);
   QCOMPARE(StringUtils::commonPrefix(s1, s6), 0);
   QCOMPARE(StringUtils::commonPrefix(s1, s7), 4);
   QCOMPARE(StringUtils::commonPrefix(s1, s8), 0);
}

void Tests::nCombinations()
{
   QCOMPARE(Global::nCombinations(5, 4), 5);
   QCOMPARE(Global::nCombinations(4, 2), 6);
   QCOMPARE(Global::nCombinations(4, 4), 1);
   QCOMPARE(Global::nCombinations(42, 6), 5245786);
}

void Tests::nbChunks()
{
   QCOMPARE(Global::nbChunks(0), 0);
   QCOMPARE(Global::nbChunks(1), 1);
   QCOMPARE(Global::nbChunks(Constants::CHUNK_SIZE - 1), 1);
   QCOMPARE(Global::nbChunks(Constants::CHUNK_SIZE), 1);
   QCOMPARE(Global::nbChunks(Constants::CHUNK_SIZE + 2), 2);
}

void Tests::formatByteSize()
{
   QCOMPARE(Global::formatByteSize(-42), QString("0 B"));
   QCOMPARE(Global::formatByteSize(0), QString("0 B"));
   QCOMPARE(Global::formatByteSize(42), QString("42 B"));
   QCOMPARE(Global::formatByteSize(1023), QString("1023 B"));
   QCOMPARE(Global::formatByteSize(1024), QString("1.0 KiB"));
   QCOMPARE(Global::formatByteSize(1484), QString("1.4 KiB"));
   QCOMPARE(Global::formatByteSize(1485), QString("1.5 KiB"));
   QCOMPARE(Global::formatByteSize(1996), QString("1.9 KiB"));
   QCOMPARE(Global::formatByteSize(1997), QString("2.0 KiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1484), QString("1.4 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1485), QString("1.5 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1996), QString("1.9 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1997), QString("2.0 MiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1024 * 1024 * 1024 * 1024 * 1024), QString("42.0 PiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1020 * 1024 * 1024 * 1024 * 1024), QString("41.8 PiB"));


   QCOMPARE(Global::formatByteSize(-42, 2), QString("0 B"));
   QCOMPARE(Global::formatByteSize(0, 2), QString("0 B"));
   QCOMPARE(Global::formatByteSize(42, 2), QString("42 B"));
   QCOMPARE(Global::formatByteSize(1023, 2), QString("1023 B"));
   QCOMPARE(Global::formatByteSize(1024, 2), QString("1.00 KiB"));
   QCOMPARE(Global::formatByteSize(1484, 2), QString("1.45 KiB"));
   QCOMPARE(Global::formatByteSize(1485, 2), QString("1.45 KiB"));
   QCOMPARE(Global::formatByteSize(1996, 2), QString("1.95 KiB"));
   QCOMPARE(Global::formatByteSize(1997, 2), QString("1.95 KiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1484, 2), QString("1.45 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1485, 2), QString("1.45 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1996, 2), QString("1.95 MiB"));
   QCOMPARE(Global::formatByteSize(1024 * 1997, 2), QString("1.95 MiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1024 * 1024 * 1024 * 1024 * 1024, 2), QString("42.00 PiB"));
   QCOMPARE(Global::formatByteSize(42LL * 1020 * 1024 * 1024 * 1024 * 1024, 2), QString("41.84 PiB"));
}

void Tests::formatTime()
{
   QCOMPARE(Global::formatTime(0), QString(""));
   QCOMPARE(Global::formatTime(60), QString("1m"));
   QCOMPARE(Global::formatTime(120), QString("2m"));
   QCOMPARE(Global::formatTime(160), QString("2m 40s"));
   QCOMPARE(Global::formatTime(12312411LL), QString("5M"));
   QCOMPARE(Global::formatTime(1243151412LL), QString("42y 9M"));
}

void Tests::availableDiskSpace()
{
   qDebug() << "Available disk space [Mo] : " << Global::availableDiskSpace(".") / 1024 / 1024;
}

void Tests::splitInWords()
{
   QCOMPARE(StringUtils::splitInWords("a"), QStringList() << "a");
   QCOMPARE(StringUtils::splitInWords("a b"), QStringList() << "a" << "b");
   QCOMPARE(StringUtils::splitInWords("    a    b    "), QStringList() << "a" << "b");
   QCOMPARE(StringUtils::splitInWords("a_b"), QStringList() << "a" << "b");
   QCOMPARE(StringUtils::splitInWords("ABC DEF"), QStringList() << "abc" << "def");
   QCOMPARE(StringUtils::splitInWords("abc%_-[]def"), QStringList() << "abc" << "def");

   // Words with accents.
   QCOMPARE(StringUtils::splitInWords("ÿÀÃé"), QStringList() << "yaae");
   QCOMPARE(StringUtils::splitInWords("àšř"), QStringList() << "asr");
}

void Tests::hashStringToInt()
{
   QCOMPARE(StringUtils::hashStringToInt(""), 0u);
   QCOMPARE(StringUtils::hashStringToInt("abcde"), 444281822u);
   QCOMPARE(StringUtils::hashStringToInt("abcdef"), 3174932005u);
}

// Test of 'Common::Path' class.
void Tests::path()
{
   Path p1;
   QCOMPARE(p1.toString(), QString());
   QCOMPARE(p1.isFile(), false);
   QCOMPARE(p1.isAbsolute(), false);
   QCOMPARE(p1.getRoot(), QString());
   QCOMPARE(p1.getDirs(), QStringList());
   QCOMPARE(p1.getFilename(), QString());
   QCOMPARE(p1.getExtension(), QString());
   QCOMPARE(p1.getLastDir(), QString());
   QCOMPARE(p1.getLastElement(), QString());
   QCOMPARE(p1.getLastElement(true), QString());

   Path p2(QString(""));
   QCOMPARE(p2.toString(), QString());
   QCOMPARE(p2.isFile(), false);
   QCOMPARE(p2.isAbsolute(), false);
   QCOMPARE(p2.getRoot(), QString());
   QCOMPARE(p2.getDirs(), QStringList());
   QCOMPARE(p2.getFilename(), QString());
   QCOMPARE(p2.getExtension(), QString());
   QCOMPARE(p2.getLastDir(), QString());
   QCOMPARE(p2.getLastElement(), QString());
   QCOMPARE(p2.getLastElement(true), QString());

   Path p3(QString("/"));
   QCOMPARE(p3.toString(), QString("/"));
   QCOMPARE(p3.isFile(), false);
   QCOMPARE(p3.isAbsolute(), true);
   QCOMPARE(p3.getRoot(), QString("/"));
   QCOMPARE(p3.getDirs(), QStringList());
   QCOMPARE(p3.getFilename(), QString(""));
   QCOMPARE(p3.getExtension(), QString(""));
   QCOMPARE(p3.getLastDir(), QString());
   QCOMPARE(p3.getLastElement(), QString());
   QCOMPARE(p3.getLastElement(true), QString("/"));

   Path p4(QString("/tmp/dir/"));
   QCOMPARE(p4.toString(), QString("/tmp/dir/"));
   QCOMPARE(p4.isFile(), false);
   QCOMPARE(p4.isAbsolute(), true);
   QCOMPARE(p4.getRoot(), QString("/"));
   QCOMPARE(p4.getDirs(), (QStringList{ "tmp", "dir" }));
   QCOMPARE(p4.getFilename(), QString(""));
   QCOMPARE(p4.getExtension(), QString(""));
   QCOMPARE(p4.getLastDir(), QString("dir"));
   QCOMPARE(p4.getLastElement(), QString("dir"));
   QCOMPARE(p4.getLastElement(true), QString("dir"));

   Path p5(QString("/tmp/dir/file.txt"));
   QCOMPARE(p5.toString(), QString("/tmp/dir/file.txt"));
   QCOMPARE(p5.isFile(), true);
   QCOMPARE(p5.isAbsolute(), true);
   QCOMPARE(p5.getRoot(), QString("/"));
   QCOMPARE(p5.getDirs(), (QStringList{ "tmp", "dir" }));
   QCOMPARE(p5.getFilename(), QString("file.txt"));
   QCOMPARE(p5.getExtension(), QString("txt"));
   QCOMPARE(p5.getLastDir(), QString("dir"));
   QCOMPARE(p5.getLastElement(), QString("file.txt"));
   QCOMPARE(p5.getLastElement(true), QString("file.txt"));

   Path p6(QString("C:/tmp/dir/file.txt"));
   QCOMPARE(p6.toString(), QString("C:/tmp/dir/file.txt"));
   QCOMPARE(p6.isFile(), true);
   QCOMPARE(p6.isAbsolute(), true);
   QCOMPARE(p6.getRoot(), QString("C:/"));
   QCOMPARE(p6.getDirs(), (QStringList{ "tmp", "dir" }));
   QCOMPARE(p6.getFilename(), QString("file.txt"));
   QCOMPARE(p6.getExtension(), QString("txt"));
   QCOMPARE(p6.getLastDir(), QString("dir"));
   QCOMPARE(p6.getLastElement(), QString("file.txt"));
   QCOMPARE(p6.getLastElement(true), QString("file.txt"));

   Path p7(QString("dir/.file.txt")); // (Hidden file).
   QCOMPARE(p7.toString(), QString("dir/.file.txt"));
   QCOMPARE(p7.isFile(), true);
   QCOMPARE(p7.isAbsolute(), false);
   QCOMPARE(p7.getRoot(), QString());
   QCOMPARE(p7.getDirs(), (QStringList{ "dir" }));
   QCOMPARE(p7.getFilename(), QString(".file.txt"));
   QCOMPARE(p7.getExtension(), QString("txt"));
   QCOMPARE(p7.getLastDir(), QString("dir"));
   QCOMPARE(p7.getLastElement(), QString(".file.txt"));
   QCOMPARE(p7.getLastElement(true), QString(".file.txt"));

   Path p7b(QString("file.txt")); // (simple file).
   QCOMPARE(p7b.toString(), QString("file.txt"));
   QCOMPARE(p7b.isFile(), true);
   QCOMPARE(p7b.isAbsolute(), false);
   QCOMPARE(p7b.getRoot(), QString());
   QCOMPARE(p7b.getDirs(), (QStringList()));
   QCOMPARE(p7b.getFilename(), QString("file.txt"));
   QCOMPARE(p7b.getExtension(), QString("txt"));
   QCOMPARE(p7b.getLastDir(), QString());
   QCOMPARE(p7b.getLastElement(), QString("file.txt"));
   QCOMPARE(p7b.getLastElement(true), QString("file.txt"));

   Path p8 = p4;
   Path p9 = p6;
   Path p10 = p7;
   QVERIFY(p8 == p4);
   QVERIFY(p9 == p6);
   QVERIFY(p10 == p7);

   QVERIFY(p4.isSameDir(p5));
   QVERIFY(!p4.isSameDir(Path("tmp/dir/")));
   QVERIFY(!p5.isSameDir(p6));

   QVERIFY(!p4.isSubOf(p4));
   QVERIFY(!p4.isSuperOf(p4));

   QVERIFY(Path().isNull());
   QVERIFY(Path("").isNull());
   QVERIFY(Path("  ").isNull());

   QVERIFY(Path("/tmp/dir1/dir2/").isSubOf(Path("/tmp/dir1/")));
   QVERIFY(Path("/tmp/").isSubOf(Path("/")));
   QVERIFY(!Path("/tmp/dir1/").isSubOf(Path("/tmp/dir1/dir2/")));
   QVERIFY(!Path("/").isSubOf(Path("/tmp/")));

   QVERIFY(Path("/tmp/dir1/").isSuperOf(Path("/tmp/dir1/dir2/")));
   QVERIFY(Path("/").isSuperOf(Path("/tmp/")));
   QVERIFY(!Path("/tmp/dir1/dir2/").isSuperOf(Path("/tmp/dir1/")));
   QVERIFY(!Path("/tmp/").isSuperOf(Path("/")));

   QVERIFY(Path("/tmp/dir1/dir2/a.txt").isSubOf(Path("/tmp/dir1/")));
   QVERIFY(Path("/tmp/a.txt").isSubOf(Path("/")));
   QVERIFY(Path("/a.txt").isSubOf(Path("/")));
   QVERIFY(!Path("/tmp/dir1/a.txt").isSubOf(Path("/tmp/dir1/dir2/")));
   QVERIFY(!Path("/a.txt").isSubOf(Path("/tmp/")));

   QVERIFY(!Path("/tmp/dir1/file.txt").removeFilename().isFile());
   QCOMPARE(Path("/tmp/dir1/file.txt").removeFilename().getDirs().last(), QString("dir1"));

   QVERIFY(Path("/tmp/dir1/file.txt").removeLastDir().isFile());
   QCOMPARE(Path("/tmp/dir1/file.txt").removeLastDir().getDirs().last(), QString("tmp"));

   QVERIFY(!Path("/tmp/dir1/file.txt").removeLastElement().isFile());
   QCOMPARE(Path("/tmp/dir1/file.txt").removeLastElement(), Path("/tmp/dir1/"));

   QCOMPARE(Path("/tmp/dir1/").removeLastElement(), Path("/tmp/"));
   QCOMPARE(Path("/tmp/").removeLastElement(), Path("/"));
   QCOMPARE(Path("/").removeLastElement(), Path("/"));

   QVERIFY(Path("/tmp/dir1/").setFilename("file.txt").isFile());
   QVERIFY(Path("/tmp/dir1/file.txt").setFilename("file2.txt").isFile());
   QCOMPARE(Path("/tmp/dir1/file.txt").setFilename("file2.txt").getFilename(), QString("file2.txt"));

   QCOMPARE(Path("/tmp/dir1/").append(Path("dir2/")), Path("/tmp/dir1/dir2/"));
   QCOMPARE(Path("/tmp/dir1/").append(Path("/dir2/")), Path("/tmp/dir1/dir2/"));
   QCOMPARE(Path("/tmp/dir1/").append(Path("file1")), Path("/tmp/dir1/file1"));
   QCOMPARE(Path("/").append(Path("file1")), Path("/file1"));
   QCOMPARE(Path().append(Path("file1")), Path("file1"));

   QCOMPARE(Path("/tmp/dir1/").prepend(Path("dir2/")), Path("dir2/tmp/dir1/"));
   QCOMPARE(Path("tmp/dir1/file.txt").prepend(Path("/dir2/")), Path("/dir2/tmp/dir1/file.txt"));

   QCOMPARE(Path("/tmp/dir1/file.txt").appendDir("dir2"), Path("/tmp/dir1/dir2/file.txt"));
   QCOMPARE(Path("").appendDir("dir"), Path("dir/"));
   QCOMPARE(Path("dir/").appendDir("dir2"), Path("dir/dir2/"));

   QCOMPARE(Path("/tmp/dir1/file.txt").prependDir("dir2"), Path("/dir2/tmp/dir1/file.txt"));
   QCOMPARE(Path("").prependDir("dir"), Path("dir/"));
   QCOMPARE(Path("dir/").prependDir("dir2"), Path("dir2/dir/"));

   const Path uncRoot("//server/share/");
   QVERIFY(uncRoot.isAbsolute());
   QVERIFY(!uncRoot.isFile());
   QCOMPARE(uncRoot.getRoot(), QString("//server/share/"));
   QVERIFY(uncRoot.getDirs().isEmpty());
   QCOMPARE(uncRoot.toString(), QString("//server/share/"));
   QCOMPARE(Path("//server/share"), uncRoot);
   QCOMPARE(uncRoot.removeLastDir(), uncRoot);
   QCOMPARE(Path("//server/share/").removeLastElement(), uncRoot);

   const Path uncFile("//server/share/folder/file.txt");
   QCOMPARE(uncFile.toString(), QString("//server/share/folder/file.txt"));
   QCOMPARE(uncFile.getRoot(), uncRoot.getRoot());
   QCOMPARE(uncFile.getDirs(), QStringList{"folder"});
   QCOMPARE(uncFile.getFilename(), QString("file.txt"));
   QCOMPARE(uncFile.removeFilename().removeLastElement(), uncRoot);
   QVERIFY(uncFile.isSubOf(uncRoot));
   QVERIFY(uncRoot.isSuperOf(uncFile));
   QVERIFY(!uncFile.isSubOf(Path("//server/other/")));
   QVERIFY(!uncFile.isSubOf(Path("//other/share/")));
   QVERIFY(!uncFile.isSubOf(Path("/server/share/")));
   QCOMPARE(uncRoot.append(Path("folder/file.txt")), uncFile);
   QCOMPARE(Path(uncFile.toString()), uncFile);
   QCOMPARE(Path("//server/share/folder/../"), uncRoot);
   QCOMPARE(Path("//server/share/../../"), uncRoot);
   QCOMPARE(Path("//server/share/../file.txt"), uncRoot.setFilename("file.txt"));
   QCOMPARE(Path("//server/share/./folder//file.txt"), uncFile);
#ifdef Q_OS_WIN
   QCOMPARE(Path(QStringLiteral("\\\\server\\share")), uncRoot);
   QCOMPARE(Path(QStringLiteral("\\\\server\\share\\folder\\file.txt")), uncFile);
   QCOMPARE(Path(QStringLiteral("\\\\server\\share\\folder\\")).removeLastElement(), uncRoot);
#endif

   // Composed paths must have the same components and containment as parsed paths.
   const Path shared("C:/shared/");
   const Path outside("../outside.txt");
   const Path combined = shared.append(outside);
   QCOMPARE(combined, Path("C:/outside.txt"));
   QVERIFY(!combined.isSubOf(shared));
   QCOMPARE(Path("C:/shared/").append(Path("../outside.txt")), combined);
   QCOMPARE(outside.prepend(shared), combined);
   QCOMPARE(Path("../outside.txt").prepend(Path("C:/shared/")), combined);
   QCOMPARE(Path(combined.toString()), combined);
   QCOMPARE(shared.appendDir(".."), Path("C:/"));
   QCOMPARE(Path("C:/shared/").appendDir(".."), Path("C:/"));
   QCOMPARE(uncRoot.append(Path("../../file.txt")), Path("//server/share/file.txt"));
   QCOMPARE(Path("/").append(Path("../../file.txt")), Path("/file.txt"));
   QCOMPARE(Path("dir/").append(Path("../../file.txt")), Path("../file.txt"));
   QCOMPARE(Path("dir/").appendDir(".."), Path("."));
   QCOMPARE(Path("./").append(Path("file.txt")), Path("file.txt"));
   QCOMPARE(shared.appendDir("."), shared);
   QCOMPARE(shared.appendDir(""), shared);
   const Path relativeParent("../file.txt");
   QCOMPARE(relativeParent.prependDir("dir"), Path("file.txt"));
   QCOMPARE(Path("../file.txt").prependDir("dir"), Path("file.txt"));
   QCOMPARE(Path(QStringList{"a", "", ".", "b", ".."}), Path("a/"));
   QCOMPARE(Path(QStringList{"a", "..", ".."}), Path("../"));
   QCOMPARE(Path(QStringList{"a", ".."}), Path("."));
   QVERIFY(Path(QStringList{}).isNull());
   QVERIFY(Path(QStringList{""}).isNull());

   // Single-component APIs reject paths instead of storing embedded separators.
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, shared.appendDir("a/b"));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, Path("C:/shared/").appendDir("a/b"));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, shared.prependDir("a/b"));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, Path("C:/shared/").prependDir("a/b"));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, shared.setFilename("../outside.txt"));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, Path("C:/shared/").setFilename("../outside.txt"));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, shared.setFilename("."));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, shared.setFilename(".."));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, Path(QStringList{"a/b"}));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, Path(QStringList{"C:"}));
   QCOMPARE(shared.setFilename(""), shared);
#ifdef Q_OS_WIN
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, shared.appendDir(QStringLiteral("a\\b")));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, shared.setFilename(QStringLiteral("a\\b")));
#else
   QCOMPARE(shared.appendDir(QStringLiteral("a\\b")).getLastDir(), QStringLiteral("a\\b"));
#endif
}

void Tests::sortedList()
{
   SortedList<int> list;

   list.insert(16);
   list.insert(2);
   list.insert(9);
   list.insert(10);
   list.insert(3);

   auto test =
      [&](const QList<int>& expected)
      {
         int i = 0;
         foreach (int n, list.getList())
            QCOMPARE(n, expected[i++]);
      };

   test(QList<int> { 2, 3, 9, 10, 16 });

   list.insert(QList<int> { 1, 4, 5, 12, 15, 32 });

   test(QList<int> { 1, 2, 3, 4, 5, 9, 10, 12, 15, 16, 32 });

   list.removeOne(32);
   list.removeOne(2);
   list.removeOne(3);
   list.removeOne(12);
   list.removeOne(42);

   test(QList<int> { 1, 4, 5, 9, 10, 15, 16, });

   list.clear();

   QVERIFY(list.getList().isEmpty());
}

void Tests::sortedArray()
{
   // Insert 75 ASCII characters.
   QList<char> orderedList;
   for (char c = '0'; c <= 'z'; c++)
      orderedList << c;

   // Repeate the tests 1000 times with a random insert order and a random remove order each time.
   for (int seed = 1; seed <= 1000; seed++)
   {
      QRandomGenerator64 rng(seed);

      SortedArray<char, 5> array;

      // Insert the elements in a pseudo random order.
      while (array.size() != orderedList.size())
         array.insert(orderedList[rng.bounded(orderedList.size())]);

      // Test if all values are known.
      for (int i = 0; i < array.size(); i++)
         QVERIFY(orderedList.contains(array.getFromValue(orderedList[i])));

      // Access with integer index.
      for (int i = 0; i < array.size(); i++)
         QCOMPARE(array.getFromIndex(i), orderedList[i]);

      // Iterator.
      QList<char>::iterator j = orderedList.begin();
      for (SortedArray<char, 5>::iterator i = array.begin(); i != array.end() && j != orderedList.end(); ++i, ++j)
         QCOMPARE(*i, *j);

      int unknownElement = array.indexOf('*');
      QCOMPARE(unknownElement, -1);

      int first = array.indexOfNearest('*');
      QCOMPARE(first, 0);

      int last = array.indexOfNearest('~');
      QCOMPARE(last, array.size() - 1);

      char firstChar = *array.iteratorOfNearest('*');
      QCOMPARE(firstChar, '0');

      // Remove the elements in a pseudo random order.
      while (array.size() != 0)
      {
         const char letter = orderedList[rng.bounded(orderedList.size())];
         array.remove(letter);
         QVERIFY(!array.contains(letter));
      }
   }

   {
      // Test other comparison functions.
      QList<QString> values { "albinos", "Andrew", "double", "David" };
      QList<QString> res1 { "Andrew", "David", "albinos", "double" };
      QList<QString> res2 { "albinos", "Andrew", "David", "double" };
      SortedArray<QString> array;

      for (int i = 0; i < values.size(); i++)
         array.insert(values[i]);

      qDebug() << "Sorted values without a sorted function defined:";
      for (int i = 0; i < array.size(); i++)
      {
         QCOMPARE(array.getFromIndex(i), res1[i]);
         qDebug() << array.getFromIndex(i);
      }

      array.setSortedFunction(
         [](const QString& s1, const QString& s2) { return s1.toLower() < s2.toLower(); }
      );

      qDebug() << "Sorted values with a sorted function defined:";
      for (int i = 0; i < array.size(); i++)
      {
         QCOMPARE(array.getFromIndex(i), res2[i]);
         qDebug() << array.getFromIndex(i);
      }

      array.insert("actual");
      QCOMPARE(array.getFromIndex(0), QString("actual"));

      QCOMPARE(array.indexOfNearest("aligator"), 1);

      qDebug() << "Sorted values using an iterator:";
      auto i = array.begin();
      QCOMPARE(*i, QString("actual"));
      while (i != array.end())
         qDebug() << *(i++);

      QCOMPARE(*array.iteratorOfNearest("aligator"), "albinos");
      QCOMPARE(*array.iteratorOfNearest("Aaron"), "actual");
      QCOMPARE(*array.iteratorOfNearest("Zorro"), "double");
      QCOMPARE(++array.iteratorOfNearest("Zorro"), array.end());
   }

   {
      SortedArray<int> array;
      SortedArray<int>::iterator begin = array.begin();
      SortedArray<int>::iterator end = array.end();
      QCOMPARE(begin, end);

      if (!array.isEmpty())
         QFAIL("The array is not empty!");

      // Test the C++11 range-for-statement.
      array.insert(7);
      array.insert(3);
      array.insert(9);
      array.insert(2);
      QString result;
      for (int a : array)
         result.append(QString::number(a)).append(' ');
      QCOMPARE(result, "2 3 7 9 ");
   }
}

namespace
{
   struct SortedArrayCopyItem
   {
      int key = 0;
      std::shared_ptr<int> resource;
      std::shared_ptr<int> copiesBeforeThrow;

      bool operator<(const SortedArrayCopyItem& other) const { return key < other.key; }

      SortedArrayCopyItem& operator=(const SortedArrayCopyItem& other)
      {
         if (other.copiesBeforeThrow && *other.copiesBeforeThrow >= 0)
         {
            if (*other.copiesBeforeThrow == 0)
               throw std::runtime_error("Element copy failed");
            --*other.copiesBeforeThrow;
         }
         key = other.key;
         resource = other.resource;
         copiesBeforeThrow = other.copiesBeforeThrow;
         return *this;
      }
   };
}

void Tests::sortedArrayCopy()
{
   // Small nodes force a tree with several levels and exercise parent links.
   SortedArray<SortedArrayCopyItem, 3> original;
   std::vector<std::shared_ptr<int>> resources;
   for (int i = 0; i < 40; ++i)
   {
      resources.push_back(std::make_shared<int>(i));
      original.insert(SortedArrayCopyItem { i, resources.back(), {} });
   }

   for (bool useAssignment : { false, true })
   {
      auto copy = useAssignment ? SortedArray<SortedArrayCopyItem, 3>() : original;
      if (useAssignment)
         copy = original;
      for (const auto& resource : resources)
         QCOMPARE(resource.use_count(), 2L); // Copies initially share their tree.

      copy.insert(SortedArrayCopyItem { 40, {}, {} }); // Force detachment.
      for (const auto& resource : resources)
         QCOMPARE(resource.use_count(), 3L);

      int expectedKey = 0;
      for (const auto& item : copy)
         QCOMPARE(item.key, expectedKey++);
      QCOMPARE(expectedKey, 41);
      QCOMPARE(original.size(), 40);
      QVERIFY(!original.contains(SortedArrayCopyItem { 40, {}, {} }));

      QVERIFY(copy.remove(SortedArrayCopyItem { 0, {}, {} }));
      QVERIFY(original.contains(SortedArrayCopyItem { 0, {}, {} }));
      QCOMPARE(resources.front().use_count(), 2L);
   }

   for (const auto& resource : resources)
      QCOMPARE(resource.use_count(), 2L);
   original.clear();
   for (const auto& resource : resources)
      QCOMPARE(resource.use_count(), 1L);
}

void Tests::sortedArrayCopyException()
{
   SortedArray<SortedArrayCopyItem, 3> original;
   auto copiesBeforeThrow = std::make_shared<int>(-1);
   std::vector<std::shared_ptr<int>> resources;
   for (int i = 0; i < 40; ++i)
   {
      resources.push_back(std::make_shared<int>(i));
      original.insert(SortedArrayCopyItem { i, resources.back(), copiesBeforeThrow });
   }
   auto copy = original;
   const SortedArrayCopyItem key { 0, {}, {} };
   const auto initialBudgetOwners = copiesBeforeThrow.use_count();

   // Fail at every element in the recursive copy, including after complete
   // child subtrees have been copied. No copied resources may survive a failure.
   for (int successfulCopies = 0; successfulCopies < 40; ++successfulCopies)
   {
      *copiesBeforeThrow = successfulCopies;
      QVERIFY_THROWS_EXCEPTION(std::runtime_error, copy.getFromValue(key));
      QCOMPARE(copiesBeforeThrow.use_count(), initialBudgetOwners);
      for (const auto& resource : resources)
         QCOMPARE(resource.use_count(), 2L);
      QCOMPARE(copy.size(), original.size());
      int expectedKey = 0;
      for (const auto& item : copy)
         QCOMPARE(item.key, expectedKey++);
      QCOMPARE(expectedKey, 40);
   }

   // Both arrays remain usable after a failed detachment.
   *copiesBeforeThrow = -1;
   copy.getFromValue(key);
   for (const auto& resource : resources)
      QCOMPARE(resource.use_count(), 3L);
   original.clear();
   QCOMPARE(*copy.getFromValue(key).resource, 0);
   copy.clear();
   for (const auto& resource : resources)
      QCOMPARE(resource.use_count(), 1L);
}

void Tests::sortedArrayIndexedCopyOnWrite()
{
   SortedArray<SortedArrayCopyItem, 3> original;
   for (int i = 0; i < 40; ++i)
      original.insert(SortedArrayCopyItem { i, std::make_shared<int>(i), {} });

   auto copy = original;
   const auto& constOriginal = original;
   const auto& constCopy = copy;

   // Const indexed reads preserve sharing, even in a multilevel tree.
   for (int i = 0; i < original.size(); ++i)
      QCOMPARE(&constOriginal.getFromIndex(i), &constCopy.getFromIndex(i));

   // Change payloads, preserving the keys used to order the elements.
   for (int i = 0; i < copy.size(); ++i)
   {
      copy.getFromIndex(i).resource = std::make_shared<int>(100 + i);
      QCOMPARE(*constOriginal.getFromIndex(i).resource, i);
      QCOMPARE(*constCopy.getFromIndex(i).resource, 100 + i);
      QCOMPARE(constCopy.getFromIndex(i).key, i);
      QVERIFY(&constOriginal.getFromIndex(i) != &constCopy.getFromIndex(i));
   }

   // MapArray exposes the same mutable indexed access for its mapped values.
   MapArray<int, QString> map;
   map.insert(1, QString("original"));
   auto mapCopy = map;
   mapCopy.getValueFromIndex(0) = "changed";
   QCOMPARE(map.getValueFromIndex(0), QString("original"));
   QCOMPARE(mapCopy.getValueFromIndex(0), QString("changed"));
}

void Tests::sortedArrayConstIterator()
{
   SortedArray<SortedArrayCopyItem> original;
   original.insert(SortedArrayCopyItem { 1, std::make_shared<int>(10), {} });
   auto copy = original;
   const auto& constCopy = copy;

   // Both iterator access operators must expose const elements, including
   // iterators obtained from a non-const array, since iteration never detaches.
   static_assert(std::is_same<decltype(copy.begin().operator->()), const SortedArrayCopyItem*>::value,
      "Iterator arrow access must not allow modification of shared elements");
   static_assert(std::is_same<decltype(constCopy.begin().operator->()), const SortedArrayCopyItem*>::value,
      "Const array iterators must return pointers to const elements");
   static_assert(std::is_same<decltype(*constCopy.begin()), const SortedArrayCopyItem&>::value,
      "Iterator dereference must return a const reference");

   QCOMPARE(constCopy.begin()->key, 1);
   QCOMPARE(*constCopy.begin()->resource, 10);
   QCOMPARE(constCopy.begin().operator->(), original.begin().operator->());
   QCOMPARE(&*constCopy.begin(), constCopy.begin().operator->());
}

void Tests::sortedArraySubscriptCascadingSplit()
{
   SortedArray<SortedArrayCopyItem, 3> array;
   // Inserting 80 promotes it above the original leaf's immediate parent.
   const int keys[] { 45, 49, 67, 71, 72, 15, 90, 29, 1, 97, 81, 100, 25, 5, 80 };
   int expectedSize = 0;
   for (int key : keys)
   {
      const SortedArrayCopyItem value { key, {}, {} };
      auto& inserted = array[value];
      QCOMPARE(&inserted, &array.getFromValue(value));
      QCOMPARE(inserted.key, key);
      QCOMPARE(array.size(), ++expectedSize);
      inserted.resource = std::make_shared<int>(key * 10);
   }

   // Existing-key access returns the stored element without inserting again;
   // payloads written through earlier references survive subsequent splits.
   for (int key : keys)
   {
      const SortedArrayCopyItem value { key, {}, {} };
      auto& existing = array[value];
      QCOMPARE(&existing, &array.getFromValue(value));
      QVERIFY(existing.resource);
      QCOMPARE(*existing.resource, key * 10);
      QCOMPARE(array.size(), expectedSize);
   }
}

void Tests::sortedArrayInternalNodeIndices()
{
   // The middle value is promoted into the root by the third insertion.
   SortedArray<int, 3> small;
   for (int value : { 1, 2, 3 })
      small.insert(value);
   QCOMPARE(small.indexOf(2), 1);
   QCOMPARE(small.indexOfNearest(2), 1);

   for (bool descending : { false, true })
   {
      SortedArray<int> array;
      if (descending)
         array.setSortedFunction([](int a, int b) { return a > b; });

      // A deterministic permutation creates several levels of internal nodes.
      // Even keys leave gaps for queries that must descend to a leaf.
      for (int i = 0; i < 80; ++i)
         array.insert(2 * ((i * 37) % 80));
      QList<int> expected;
      for (int i = 0; i < 80; ++i)
         expected.append(2 * (descending ? 79 - i : i));

      // Validate ranks again as removals redistribute and merge internal nodes.
      for (int step = 0; step <= 40; ++step)
      {
         if (step > 0)
         {
            const int removed = 4 * (step - 1);
            QVERIFY(array.remove(removed));
            expected.removeOne(removed);
         }

         QCOMPARE(array.size(), expected.size());
         for (int i = 0; i < expected.size(); ++i)
         {
            QCOMPARE(array.getFromIndex(i), expected[i]);
            QCOMPARE(array.indexOf(expected[i]), i);
            QCOMPARE(array.indexOfNearest(expected[i]), i);

            const int inGap = expected[i] + (descending ? -1 : 1);
            QCOMPARE(array.indexOf(inGap), -1);
            QCOMPARE(array.indexOfNearest(inGap), i);
         }
         QCOMPARE(array.indexOfNearest(descending ? 160 : -2), 0);
      }
   }
}

void Tests::sortedArrayInsertIndex()
{
   for (bool descending : { false, true })
   {
      SortedArray<int, 3> array;
      const auto less = [descending](int a, int b) { return descending ? a > b : a < b; };
      array.setSortedFunction(less);
      QList<int> expected;
      for (int i = 0; i < 80; ++i)
      {
         const int value = (i * 37) % 80;
         int index = 0;
         while (index < expected.size() && less(expected[index], value))
            ++index;
         expected.insert(index, value);
         bool exists = true;
         QCOMPARE(array.insert(value, &exists), index);
         QVERIFY(!exists);
         QCOMPARE(array.getFromIndex(index), value);
      }
      for (int i = 0; i < expected.size(); ++i)
      {
         bool exists = false;
         QCOMPARE(array.insert(expected[i], &exists), i);
         QVERIFY(exists);
         QCOMPARE(array.size(), expected.size());
      }
   }

   // Looking up the argument after insertion would use a moved-from value.
   SortedArray<QString> strings;
   QCOMPARE(strings.insert(QString("alpha")), 0);
   QString last("zulu");
   QCOMPARE(strings.insert(std::move(last)), 1);
   QCOMPARE(strings.getFromIndex(1), QString("zulu"));

   // MapArray forwards the index and must also return it when replacing payloads.
   MapArray<QString, QString> map;
   QCOMPARE(map.insert(QString("alpha"), QString("first")), 0);
   QCOMPARE(map.insert(QString("zulu"), QString("last")), 1);
   QCOMPARE(map.insert(QString("middle"), QString("old")), 1);
   bool exists = false;
   QCOMPARE(map.insert(QString("middle"), QString("updated"), &exists), 1);
   QVERIFY(exists);
   QCOMPARE(map.size(), 3);
   QCOMPARE(map.getValueFromIndex(1), QString("updated"));
}

void Tests::sortedArrayToList()
{
   SortedArray<int> empty;
   QVERIFY(empty.toList().isEmpty());
   empty.insert(42);
   QCOMPARE(empty.toList(), QList<int> { 42 });

   // Non-default order and enough values to traverse several tree levels.
   SortedArray<int, 3> array;
   QList<int> expected;
   for (int i = 0; i < 80; ++i)
   {
      array.insert((i * 37) % 80);
      expected.append(i);
   }
   const auto& constArray = array;
   QCOMPARE(constArray.toList(), expected);
   array.setSortedFunction([](int a, int b) { return a > b; });
   QList<int> descending;
   for (int i = 79; i >= 0; --i)
      descending.append(i);
   QCOMPARE(constArray.toList(), descending);

   // The list owns copies of nontrivial elements and survives clearing the tree.
   SortedArray<QString> strings;
   strings.insert(QString("zulu"));
   strings.insert(QString("alpha"));
   auto list = strings.toList();
   const QList<QString> words { "alpha", "zulu" };
   QCOMPARE(list, words);
   list[0] = "changed";
   QCOMPARE(strings.getFromIndex(0), QString("alpha"));
   strings.clear();
   QCOMPARE(list[0], QString("changed"));
   QCOMPARE(list[1], QString("zulu"));
}

void Tests::sortedArrayComparatorCollisions()
{
   SortedArray<QString, 3> words;
   for (const QString& word : QList<QString> { "b", "A", "a", "B" })
      words.insert(word);
   const auto original = words;
   words.setSortedFunction([](const QString& a, const QString& b) {
      return a.toLower() < b.toLower();
   });
   const QList<QString> expectedWords { "a", "b" };
   QCOMPARE(words.toList(), expectedWords);
   QCOMPARE(original.size(), 4);
   QVERIFY(words.contains("A"));
   bool exists = false;
   QCOMPARE(words.insert(QString("B"), &exists), 1);
   QVERIFY(exists);
   QCOMPARE(words.size(), 2);
   QCOMPARE(words.getFromIndex(1), QString("B"));

   // Deduplication also works when rebuilding requires multiple tree levels.
   SortedArray<int, 3> numbers;
   for (int i = 79; i >= 0; --i)
      numbers.insert(i);
   numbers.setSortedFunction([](int a, int b) { return a % 10 < b % 10; });
   QCOMPARE(numbers.size(), 10);
   for (int i = 0; i < 10; ++i)
   {
      QCOMPARE(numbers.getFromIndex(i), 70 + i);
      QCOMPARE(numbers.indexOf(i), i);
   }
   QVERIFY(numbers.remove(2));
   QVERIFY(!numbers.contains(72));
}

void Tests::sortedArrayComparatorException()
{
   SortedArray<SortedArrayCopyItem, 3> array;
   auto copiesBeforeThrow = std::make_shared<int>(-1);
   std::vector<std::shared_ptr<int>> resources;
   for (int i = 0; i < 40; ++i)
   {
      resources.push_back(std::make_shared<int>(i));
      array.insert(SortedArrayCopyItem { i, resources.back(), copiesBeforeThrow });
   }
   const auto original = array;
   const auto descending = [](const SortedArrayCopyItem& a, const SortedArrayCopyItem& b) {
      return a.key > b.key;
   };
   const auto throwingComparator = [](const SortedArrayCopyItem& a, const SortedArrayCopyItem& b) {
      // Fail on the initial lookup of 20, after a multilevel tree was built.
      if (a.key == 20 || b.key == 20)
         throw std::runtime_error("Comparison failed");
      return a.key > b.key;
   };
   QVERIFY_THROWS_EXCEPTION(std::runtime_error, array.setSortedFunction(throwingComparator));

   // Also fail while copying an element into the replacement tree.
   *copiesBeforeThrow = 1;
   QVERIFY_THROWS_EXCEPTION(std::runtime_error, array.setSortedFunction(descending));
   *copiesBeforeThrow = -1;
   QCOMPARE(array.size(), 40);
   const auto& unchanged = array;
   for (int i = 0; i < 40; ++i)
   {
      const SortedArrayCopyItem key { i, {}, {} };
      QCOMPARE(array.indexOf(key), i); // The original comparator is still active.
      QCOMPARE(unchanged.getFromIndex(i).key, i);
      QCOMPARE(&unchanged.getFromIndex(i), &original.getFromIndex(i));
      QCOMPARE(resources[i].use_count(), 2L);
   }

   // A subsequent successful change affects only this copy.
   array.setSortedFunction(descending);
   for (int i = 0; i < 40; ++i)
   {
      QCOMPARE(unchanged.getFromIndex(i).key, 39 - i);
      QCOMPARE(original.getFromIndex(i).key, i);
   }
}

void Tests::sortedArrayEmptyNearestIndex()
{
   SortedArray<int, 3> array;
   for (int value : { -1, 0, 1 })
      QCOMPARE(array.indexOfNearest(value), -1);

   array.insert(10);
   for (int value : { 9, 10, 11 })
      QCOMPARE(array.indexOfNearest(value), 0);
   QVERIFY(array.remove(10));
   QCOMPARE(array.indexOfNearest(10), -1);

   // Exercise the empty root left after collapsing a multilevel tree.
   for (int i = 0; i < 40; ++i)
      array.insert(i);
   for (int i = 0; i < 40; ++i)
      QVERIFY(array.remove(i));
   QVERIFY(array.isEmpty());
   QCOMPARE(array.indexOfNearest(20), -1);

   array.setSortedFunction([](int a, int b) { return a > b; });
   QCOMPARE(array.indexOfNearest(20), -1);
   array.insert(20);
   QCOMPARE(array.indexOfNearest(20), 0);
   array.clear();
   QCOMPARE(array.indexOfNearest(20), -1);
}

void Tests::sortedArrayComparatorConstructor()
{
   struct Item
   {
      int key = 0;
      QString payload;
      // Deliberately no operator<.
   };
   SortedArray<Item, 3> array([](const Item& a, const Item& b) { return a.key > b.key; });
   for (int i = 0; i < 40; ++i)
      array.insert(Item { i, QString::number(i) });
   for (int i = 0; i < 40; ++i)
   {
      QCOMPARE(array.getFromIndex(i).key, 39 - i);
      QCOMPARE(array.indexOf(Item { 39 - i, {} }), i);
   }
   auto copy = array;
   copy.clear();
   copy.insert(Item { 1, "one" });
   copy.insert(Item { 2, "two" });
   QCOMPARE(copy.getFromIndex(0).key, 2); // clear() retains the comparator.
   QCOMPARE(array.size(), 40);

   const std::function<bool(const Item&, const Item&)> emptyComparator;
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, (SortedArray<Item, 3>(emptyComparator)));
   QVERIFY_THROWS_EXCEPTION(std::invalid_argument, array.setSortedFunction(emptyComparator));
   QCOMPARE(array.getFromIndex(0).key, 39);
}

void Tests::sortedArrayStandardIterator()
{
   using Array = SortedArray<int, 3>;
   using Iterator = Array::iterator;
   static_assert(std::forward_iterator<Iterator>);
   static_assert(std::is_same_v<std::iterator_traits<Iterator>::value_type, int>);
   static_assert(std::is_same_v<std::iterator_traits<Iterator>::reference, const int&>);
   static_assert(std::is_same_v<std::iterator_traits<Iterator>::pointer, const int*>);
   Iterator first;
   Iterator second;
   QVERIFY(first == second);

   Array array;
   QCOMPARE(std::distance(array.begin(), array.end()), 0);
   std::vector<int> expected;
   for (int i = 0; i < 40; ++i)
   {
      array.insert((i * 17) % 40);
      expected.push_back(i);
   }
   QCOMPARE(std::distance(array.begin(), array.end()), 40);
   const std::vector<int> actual(array.begin(), array.end());
   QVERIFY(actual == expected);
   first = std::find(array.begin(), array.end(), 20);
   QCOMPARE(*first, 20);
   second = first;
   QCOMPARE(*second++, 20);
   QCOMPARE(*second, 21);
   QCOMPARE(*first, 20); // Copies can advance independently.
   first = array.end();
   QVERIFY(first == array.end());

   Array other;
   other.insert(99);
   first = other.begin(); // Assignment can rebind to another container.
   QCOMPARE(*first, 99);
}

namespace
{
   struct SortedArrayClearItem
   {
      inline static int defaultsBeforeThrow = -1;
      int key = 0;
      std::shared_ptr<int> resource;

      SortedArrayClearItem()
      {
         if (defaultsBeforeThrow == 0)
            throw std::runtime_error("Default construction failed");
         if (defaultsBeforeThrow > 0)
            --defaultsBeforeThrow;
      }
      bool operator<(const SortedArrayClearItem& other) const { return key < other.key; }
   };
}

void Tests::sortedArrayClearException()
{
   SortedArray<SortedArrayClearItem, 3> array;
   auto resource = std::make_shared<int>(42);
   SortedArrayClearItem item;
   item.key = 1;
   item.resource = resource;
   array.insert(item);
   const auto original = array;
   const auto& constArray = array;
   const auto owners = resource.use_count();

   // Fail partway through constructing the new empty root, then retry.
   bool threw = false;
   SortedArrayClearItem::defaultsBeforeThrow = 1;
   try { array.clear(); }
   catch (const std::runtime_error&) { threw = true; }
   SortedArrayClearItem::defaultsBeforeThrow = -1;
   QVERIFY(threw);
   QCOMPARE(array.size(), 1);
   QCOMPARE(&constArray.getFromIndex(0), &original.getFromIndex(0));
   QCOMPARE(resource.use_count(), owners);
   QCOMPARE(*constArray.getFromIndex(0).resource, 42);
   array.clear();
   QVERIFY(array.isEmpty());
   QCOMPARE(original.size(), 1);

   // Clearing shared data must not copy the elements that will be discarded.
   auto budget = std::make_shared<int>(-1);
   SortedArray<SortedArrayCopyItem> copying;
   copying.insert(SortedArrayCopyItem { 1, resource, budget });
   const auto retained = copying;
   *budget = 0;
   copying.clear();
   QVERIFY(copying.isEmpty());
   QCOMPARE(retained.size(), 1);
}

void Tests::mapArray()
{
   MapArray<Common::Hash, QString> array;
   const Hash h1 = Hash::fromStr("02e4a0f0e55a308eb83b00eb13023a42cbaffe770000000000000000").value();
   const QString v1("I'm V1");

   const Hash h2 = Hash::fromStr("2c583d414e4a9eb956228209b367e48f59078a4b0000000000000000").value();
   const QString v2("I'm V2");

   const Hash h3 = Hash::fromStr("db23d79ed24b1c40b1f88294f877fac03f6dd7890000000000000000").value();
   const QString v3("I'm V3");

   array.insert(h1, v1);
   array.insert(h2, v2);
   array.insert(h3, v3);

   QCOMPARE(array.size(), 3);

   QCOMPARE(array[h1], v1);
   QCOMPARE(array[h2], v2);
   QCOMPARE(array[h3], v3);

   const Hash h4 = Hash::fromStr("e8f98b5a2dd96315dfcf7e490e31b2ba6234887c0000000000000000").value();
   const QString v4("I'm V4");
   array[h4] = v4;

   QCOMPARE(array.size(), 4);

   QCOMPARE(array.getValueFromIndex(0), v1);
   QCOMPARE(array.getValueFromIndex(1), v2);
   QCOMPARE(array.getValueFromIndex(2), v3);
   QCOMPARE(array.getValueFromIndex(3), v4);

   QCOMPARE(array.getKeyFromIndex(0), h1);
   QCOMPARE(array.getKeyFromIndex(1), h2);
   QCOMPARE(array.getKeyFromIndex(2), h3);
   QCOMPARE(array.getKeyFromIndex(3), h4);

   QCOMPARE(array.indexOf(h1), 0);
   QCOMPARE(array.indexOf(h2), 1);
   QCOMPARE(array.indexOf(h3), 2);
   QCOMPARE(array.indexOf(h4), 3);

   QVERIFY(!array.remove(Hash::rand(1)));
   QVERIFY(array.remove(h1));
   QCOMPARE(array.size(), 3);
   QCOMPARE(array.getKeyFromIndex(0), h2);

   // The 'const' accessors have their own implementation, they must be instantiated as well.
   const MapArray<Common::Hash, QString>& constArray = array;
   QCOMPARE(constArray.getValueFromIndex(0), v2);
   QCOMPARE(constArray.getKeyFromIndex(0), h2);

   array.removeFromIndex(0);
   QCOMPARE(array.size(), 2);
   QCOMPARE(array.getKeyFromIndex(0), h3);

   try
   {
      array.getValueFromIndex(10);
      QFAIL("array.getValueFromIndex(10); should throw an exception");
   }
   catch (MapArray<Common::Hash, QString>::NotFoundException&)
   {
   }
}

void Tests::transferRateCalculator()
{
   QSKIP("TODO: Rewrite this test, take too much time.");

   TransferRateCalculator t;
   QCOMPARE(t.getTransferRate(), 0);

   static int N = 700;
   for (int i = 1; i < N; i += 10)
   {
      QTest::qSleep(i);
      if (i < 600)
         t.addData(i);
      qDebug() << "Transfer rate: " << t.getTransferRate();

      QVERIFY(t.getTransferRate() <= 1000);
   }

   QCOMPARE(t.getTransferRate(), 0);
}

void Tests::writePersistentData()
{
   this->hash = Hash::rand();
   Protos::Common::Hash hashMessage;
   hashMessage.set_hash(this->hash.getData(), Hash::HASH_SIZE);
   PersistentData::setValue("paul", hashMessage, Global::DataFolderType::ROAMING);
}

void Tests::readPersistentData()
{
   Protos::Common::Hash hashMessage;
   PersistentData::getValue("paul", hashMessage, Global::DataFolderType::ROAMING);
   Hash hashRead(hashMessage.hash().data());

   QVERIFY(this->hash == hashRead);

   try
   {
      PersistentData::getValue("john", hashMessage, Global::DataFolderType::ROAMING);
      QFAIL("'john' shouldn't exist");
   }
   catch (UnknownValueException)
   {
      qDebug() << "Ok, exception UnknownValueException caught for the value 'john'";
   }
   catch (...)
   {
      QFAIL("Unknown exception occurred");
   }
}

void Tests::removePersistentData()
{
   QVERIFY(PersistentData::rmValue("paul", Global::DataFolderType::ROAMING));
}

void Tests::writeSettings()
{
   this->hash = Hash::rand();

   SETTINGS.setFilename("tests_core_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());

   SETTINGS.set("nick", QString("paul"));
   SETTINGS.set("peer_id", this->hash);
   SETTINGS.save();
}

void Tests::readSettings()
{
   SETTINGS.load();

   QString nick = SETTINGS.get<QString>("nick");
   Hash hash = SETTINGS.get<Hash>("peer_id");

   QCOMPARE(nick, QString("paul"));
   QCOMPARE(hash.toStr(), this->hash.toStr());
}

void Tests::removeSettings()
{
   SETTINGS.remove();
}

void Tests::generateAHash()
{
   // Negative values are cast to 'char' because of an error of GCC 4.6 on ARM architecture:
   // "error: narrowing conversion of ‘-0x0000000000000002c’ from ‘int’ to ‘const char’ inside { } [-fpermissive]"
   const char array[Hash::HASH_SIZE] = {
       0x2d,  0x73,  0x73,  0x6f,
       0x34, (char)-0x59,  0x38,  0x37,
      (char)-0x2C,  0x22, (char)-0x09, (char)-0x55,
      (char)-0x5E,  0x74,  0x0D, (char)-0x7C,
       0x09, (char)-0x54,  0x60, (char)-0x21,
      0x3b, (char)-0xb4,  0x23, 0x11,
      0x09, (char)-0x54,  0x60, (char)-0x21
   };
   QByteArray byteArray(array, Hash::HASH_SIZE);

   qDebug() << "Reference                     : " << byteArray.toHex();;

   Hash h1 = Hash::rand();
   qDebug() << "h1 (Randomly generated hash)  : " << h1.toStr();

   Hash h2(byteArray);
   qDebug() << "h2 (from QByteArray)          : " << h2.toStr();
   QVERIFY(memcmp(h2.getData(), array, Hash::HASH_SIZE) == 0);

   Hash h3(h2);
   qDebug() << "h3 (copied from h2)           : " << h3.toStr();
   QVERIFY(memcmp(h3.getData(), array, Hash::HASH_SIZE) == 0);

   Hash h4(array);
   qDebug() << "h4 (from char[])              : " << h4.toStr();
   QVERIFY(memcmp(h4.getData(), array, Hash::HASH_SIZE) == 0);

   Hash h5;
   qDebug() << "h5 (empty)                    : " << h5.toStr();
   QVERIFY(h5.isNull());
}

void Tests::buildAnHashFromAString()
{
   const QString str("c1c7de83bacdc11ba3fcb702facbbdfb435157ceda9a4859ee230359");
   for (const QString& input : QStringList{str, str.toUpper(), QString("C1c7De83") + str.mid(8)})
   {
      const auto hash = Hash::fromStr(input);
      QVERIFY(hash.has_value());
      QCOMPARE(hash->toStr(), str);
   }

   const auto zero = Hash::fromStr(QString(2 * Hash::HASH_SIZE, QLatin1Char('0')));
   QVERIFY(zero.has_value());
   QVERIFY(zero->isNull());
}

void Tests::rejectInvalidHashStrings()
{
   const QString valid(2 * Hash::HASH_SIZE, QLatin1Char('a'));
   for (int length : {0, 1, 2, 54, 55, 57, 58, 112})
      QVERIFY(!Hash::fromStr(QString(length, QLatin1Char('a'))).has_value());

   // Check both nibbles at every byte, including non-ASCII and embedded NUL.
   for (QChar invalid : {QChar('g'), QChar('G'), QChar('/'), QChar(':'), QChar('@'),
                        QChar('`'), QChar(' '), QChar('\n'), QChar(ushort(0)),
                        QChar(ushort(0xff21)), QChar(ushort(0x0130))})
   {
      for (int i = 0; i < valid.size(); ++i)
      {
         QString input = valid;
         input[i] = invalid;
         QVERIFY(!Hash::fromStr(input).has_value());
      }
   }
   QVERIFY(!Hash::fromStr(QString(2 * Hash::HASH_SIZE, QLatin1Char('g'))).has_value());
}

void Tests::compareTwoHash()
{
   const uchar array[Hash::HASH_SIZE] = {
      0xf2, 0xb2, 0x95, 0xb4,
      0x49, 0x4a, 0x9f, 0x0d,
      0x33, 0xd9, 0x21, 0x4d,
      0x28, 0x25, 0x43, 0x80,
      0xce, 0x40, 0xb0, 0x75,
      0xdf, 0x50, 0xd5, 0xeb,
      0xa0, 0x7a, 0xb3, 0x04
   };
   QByteArray byteArray((char*)array, Hash::HASH_SIZE);
   QString str("f2b295b4494a9f0d33d9214d28254380ce40b075df50d5eba07ab304");

   Hash h1 = Hash::fromStr(str).value();
   Hash h2(byteArray);
   Hash h3 = h1;
   Hash h4;
   h4 = h1;

   QVERIFY(h1 == h1);
   QVERIFY(h1 == h2);
   QVERIFY(h1 == h3);
   QVERIFY(h1 == h4);
   QVERIFY(h2 == h3);
   QVERIFY(h2 == h4);
}

void Tests::hashMoveConstructorAndAssignment()
{
   QString str("f2b295b4494a9f0d33d9214d28254380ce40b075df50d5eba07ab304");

   // Move constructor.
   // We have to force to rValue reference because of the return optimization (http://en.wikipedia.org/wiki/Return_value_optimization).
   Hash h = std::move(Hash::fromStr(str).value());
   QVERIFY(h.toStr() == str);

   // Copy constructor
   h = std::move(Hash::fromStr(str).value());
   QVERIFY(h.toStr() == str);
}

void Tests::hasher()
{
   char str1[] = "abc";
   char str2[] = "abc";
   char str3[] = "cba";

   Hasher hasher;
   hasher.addData(str1);
   hasher.addData(str3);
   Hash h1 = hasher.getResult();

   hasher.reset();
   hasher.addData(str2);
   hasher.addData(str3);
   Hash h2 = hasher.getResult();

   hasher.reset();
   hasher.addData(str3);
   Hash h3 = hasher.getResult();

   hasher.reset();
   hasher.addSalt(42);
   hasher.addData(str1);
   Hash h4 = hasher.getResult();

   hasher.reset();
   hasher.addSalt(42);
   hasher.addData(str2);
   Hash h5 = hasher.getResult();

   QVERIFY(h1 == h2);
   QVERIFY(h1 != h3);
   QVERIFY(h2 != h3);
   QVERIFY(h4 != h1);
   QVERIFY(h4 == h5);
}

void Tests::hasherHashValue()
{
   char str1[] = "abc";
   Hasher hasher;
   hasher.addData(std::span<const char>(str1).first(sizeof(str1) - 1)); // -1 to avoid the null termination.
   Hash h1 = hasher.getResult();
   QCOMPARE(h1.toStr(), "6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6c");
}

void Tests::hasherEmptyAndSegmentedData()
{
   Hasher hasher;
   const Hash empty = hasher.getResult();
   hasher.addData(std::span<const char>());
   hasher.addData(QByteArray());
   QCOMPARE(hasher.getResult(), empty);
   QCOMPARE(Hasher::hash(QString()), empty);

   // Cross BLAKE3 block and chunk boundaries, with embedded NUL and high-bit bytes.
   QByteArray data(4097, Qt::Uninitialized);
   for (qsizetype i = 0; i < data.size(); ++i)
      data[i] = static_cast<char>(i % 256);
   hasher.addData(data);
   const Hash whole = hasher.getResult();
   hasher.addData(std::span<const char>());
   QCOMPARE(hasher.getResult(), whole);

   hasher.reset();
   const std::span<const char> bytes(data);
   hasher.addData(bytes.first(63));
   hasher.addData(bytes.subspan(63, 962));
   hasher.addData(bytes.subspan(1025));
   QCOMPARE(hasher.getResult(), whole);

   const QString text = QString::fromUtf8("a\0\xc3\xa9", 4);
   hasher.reset();
   hasher.addData(text.toUtf8());
   QCOMPARE(Hasher::hash(text), hasher.getResult());
   hasher.addSalt(42);
   QCOMPARE(Hasher::hashWithSalt(text, 42), hasher.getResult());

   hasher.reset();
   hasher.addData(std::span<const char>(whole.getData(), Hash::HASH_SIZE));
   QCOMPARE(Hasher::hash(whole), hasher.getResult());
   hasher.addSalt(42);
   QCOMPARE(Hasher::hashWithSalt(whole, 42), hasher.getResult());
}

void Tests::bloomFilter()
{
   BloomFilter bloomFilter;
   Hash h1 = Hash::rand(1);
   Hash h2 = Hash::rand(2);

   bloomFilter.add(h1);
   bloomFilter.add(h2);

   QCOMPARE(bloomFilter.test(h1), true);
   QCOMPARE(bloomFilter.test(h2), true);

   Hash h3 = Hash::rand(3);
   int nbOfFalsePositive = 0;
   const int NB_TESTS = 100; // Number of test.
   const int n = 10000; // Size of the set.

   for (int i = 0; i < NB_TESTS; i++)
   {
      bloomFilter.reset();
      for (int j = 0; j < n; j++)
         bloomFilter.add(Common::Hash::rand());

      if (bloomFilter.test(h3))
         nbOfFalsePositive++;

      if (i % 10 == 0)
         qDebug() << "i ==" << i << "...";
   }

   qDebug() << nbOfFalsePositive;
   qDebug() << "Measurement of the probability (p) for n =" << n << "with" << NB_TESTS << "tests:" << static_cast<double>(nbOfFalsePositive) / NB_TESTS;
}

void Tests::messageHeader()
{
   const uchar data[] = {
      // Type (16 bits).
      0x00, 0x01,

      // Payload size (32 bits).
      0x00, 0x00, 0x00, 0x2a,

      // Sender ID (224 bits).
      0xf2, 0xb2, 0x95, 0xb4,
      0x49, 0x4a, 0x9f, 0x0d,
      0x33, 0xd9, 0x21, 0x4d,
      0x28, 0x25, 0x43, 0x80,
      0xce, 0x40, 0xb0, 0x75,
      0xdf, 0x50, 0xd5, 0xeb,
      0xa0, 0x7a, 0xb3, 0x04
   };

   const QString peerID("f2b295b4494a9f0d33d9214d28254380ce40b075df50d5eba07ab304");

   MessageHeader header = MessageHeader::readHeader((char*)data);
   qDebug() << header.toStr();

   QVERIFY(!header.isNull());
   QCOMPARE(header.getType(), MessageHeader::CORE_IM_ALIVE);
   QCOMPARE(header.getSize(), 42u);
   QCOMPARE(header.getSenderID().toStr(), peerID);

   // We use a larger buffer to check if the last four bytes has been alterate.
   QByteArray buffer(MessageHeader::HEADER_SIZE + 4, '\0');

   MessageHeader::writeHeader(buffer.data(), header);
   QVERIFY(qstrncmp((char*)data, buffer, MessageHeader::HEADER_SIZE) == 0);
   for (int i = 0; i < 4; i++)
      QVERIFY(buffer[MessageHeader::HEADER_SIZE + i] == '\0');
}

void Tests::readAndWriteWithZeroCopyStreamQIODevice()
{
   QString filePath(QDir::tempPath().append("/test.bin"));
   QFile file(filePath);
   file.remove();

   Hash hash1 = Hash::rand(1);
   Hash hash2 = Hash::rand(2);

   qDebug() << "hash1 : " << hash1.toStr();
   qDebug() << "hash2 : " << hash2.toStr();

   Protos::Common::Hash hashMessage1;
   Protos::Common::Hash hashMessage2;

   file.open(QIODevice::WriteOnly);
   {
      ZeroCopyOutputStreamQIODevice outputStream(&file);

      hashMessage1.set_hash(hash1.getData(), Hash::HASH_SIZE);
      hashMessage1.SerializeToZeroCopyStream(&outputStream);

      hashMessage2.set_hash(hash2.getData(), Hash::HASH_SIZE);
      hashMessage2.SerializeToZeroCopyStream(&outputStream);
   }
   file.close();

   QFileInfo fileInfo(filePath);
   QCOMPARE(fileInfo.size(), static_cast<long long>(hashMessage1.ByteSizeLong() + hashMessage2.ByteSizeLong()));

   hashMessage1.Clear();
   hashMessage2.Clear();
   file.open(QIODevice::ReadOnly);
   {
      ZeroCopyInputStreamQIODevice inputStream(&file);
      hashMessage1.ParseFromBoundedZeroCopyStream(&inputStream, Hash::HASH_SIZE + 2);
      hashMessage2.ParseFromBoundedZeroCopyStream(&inputStream, Hash::HASH_SIZE + 2);
   }
   file.close();

   QCOMPARE(QByteArray(hashMessage1.hash().data(), Hash::HASH_SIZE), QByteArray(hash1.getData(), Hash::HASH_SIZE));
   QCOMPARE(QByteArray(hashMessage2.hash().data(), Hash::HASH_SIZE), QByteArray(hash2.getData(), Hash::HASH_SIZE));
}

/**
  * TODO: add some tests for these functions:
  *  - setLang(..)
  *  - getLang(..)
  *  - setIP(..)
  *  - getIP(..)
  *  - getPath(..)
  */
void Tests::protoHelper()
{
   // TODO: Rewrite.

   // const QString path("path");
   // const QString name("name");

   // Protos::Common::Entry entry;
   // entry.set_type(Protos::Common::Entry::FILE);
   // entry.set_size(0);
   // entry.set_path(path.toStdString());
   // entry.set_name(name.toStdString());

   // QCOMPARE(ProtoHelper::getStr(entry, &Protos::Common::Entry::path), path);
   // QCOMPARE(ProtoHelper::getStr(entry, &Protos::Common::Entry::name), name);

   // Protos::Common::FindPattern findPattern;
   // const QList<QString> extensions = QList<QString>() << "doc" << "txt" << "rtf";
   // foreach (QString ext, extensions)
   //    ProtoHelper::addRepeatedStr(findPattern, &Protos::Common::FindPattern::add_extension_filter, ext);
   // for (int i = 0; i < extensions.size(); i++)
   //    QCOMPARE(ProtoHelper::getRepeatedStr(findPattern, &Protos::Common::FindPattern::extension_filter, i), extensions[i]);

   // for (int i = 0; i < 5; i++)
   //    entry.add_chunk()->set_hash(Hash::rand(i).getData(), Hash::HASH_SIZE);
   // const QString debugStr = ProtoHelper::getDebugStr(entry);
   // qDebug() << endl << "The protocol buffer message (Protos::Common::Entry):" << endl << debugStr;

   // QVERIFY(debugStr.indexOf("ac2f75c043fbc36709d315f2245746d8588c3ac1") != -1);
   // QVERIFY(debugStr.indexOf("25eb8c48ff89cb854fc09081cc47edfc8619b214") != -1);
   // QVERIFY(debugStr.indexOf("a80fed48162bd24b6807a2b15f4bd52f3f1fda94") != -1);
   // QVERIFY(debugStr.indexOf("6a98f983b8c80015fd93ca6bf9a98a9577a6e094") != -1);
   // QVERIFY(debugStr.indexOf("7aaeb7c5816857c832893afc676d5e37b73968a4") != -1);

   // 'readUInt(..)' and 'readString(..)'. The first byte of a field is its protobuf tag, it's skipped.
   {
      const quint8 data[] = { 0x08, 0x96, 0x01 }; // Tag then the varint 150.
      const quint8* p = data;
      QCOMPARE(ProtoHelper::readUInt<quint32>(p, data + sizeof(data)), 150u);
      QCOMPARE(p, data + sizeof(data));
   }

   {
      const quint8 data[] = { 0x08, 0x80, 0x80, 0x80, 0x80, 0x20 }; // Tag then the varint 2^33.
      const quint8* p = data;
      QCOMPARE(ProtoHelper::readUInt<quint64>(p, data + sizeof(data)), Q_UINT64_C(8589934592));
      QCOMPARE(p, data + sizeof(data));
   }

   {
      const quint8 data[] = { 0x0A, 0x03, 'a', 'b', 'c' }; // Tag, length then the characters.
      const quint8* p = data;
      QCOMPARE(ProtoHelper::readString(p, data + sizeof(data)), QString("abc"));
      QCOMPARE(p, data + sizeof(data));
   }

   // Truncated data must be detected instead of being read past its end.
   {
      const quint8 data[] = { 0x08, 0x96 }; // The continuation bit is set but there is no next byte.
      const quint8* p = data;
      try
      {
         ProtoHelper::readUInt<quint32>(p, data + sizeof(data));
         QFAIL("readUInt(..) should throw an exception on a truncated varint");
      }
      catch (MalformedDataException&)
      {
      }
   }

   {
      const quint8 data[] = { 0x0A, 0x05, 'a', 'b' }; // Announces five characters but only two are given.
      const quint8* p = data;
      try
      {
         ProtoHelper::readString(p, data + sizeof(data));
         QFAIL("readString(..) should throw an exception on a truncated string");
      }
      catch (MalformedDataException&)
      {
      }
   }
}
