#include <Tests.h>
using namespace FileManager;

#include <QtDebug>
#include <QDir>
#include <QStringList>

#include <Protos/common.pb.h>

Tests::Tests()
{
}

void Tests::initTestCase()
{
   this->fileManager = Builder::newFileManager();
}

void Tests::addSharedDirectories()
{
   QStringList dirList;
   dirList << QDir::currentPath() + "/../../eukaryote";
   this->fileManager->setSharedDirsReadOnly(dirList);
}

void Tests::search()
{
   QTest::qSleep(1000);
   /*Protos::Common::FindResult result = this->fileManager->find("flytrap"); // It's a trap!
   qDebug() << result.DebugString().data();*/

   /*Protos::Common::FindResult result2 = this->fileManager->find("Fungi Dikarya Basidiomycota ");
   qDebug() << result2.DebugString().data();*/

   Protos::Common::FindResult result3 = this->fileManager->find("fungi agaricomycete antarctic");
   qDebug() << result3.DebugString().data();
}

void Tests::cleanupTestCase()
{
}

