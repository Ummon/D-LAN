#include "PersistantData.h"
using namespace Common;

#include <QDir>
#include <QFile>

void PersistantData::setValue(const QString& name, const QByteArray& data)
{
   if (PersistantData::createApplicationFolder())
   {
      QString tempName(name + tempPostfixTerm);
      QFile file(applicationFolderPath + QDir::separator() + tempName);
      file.open(QIODevice::WriteOnly);
      file.write(data);
      file.close();

      // TODO : Some data loss can occure here, we must remove the file first
      // because 'rename' cannot overwrite an existing file.
      QFile::remove(applicationFolderPath + QDir::separator() + name);
      file.rename(applicationFolderPath + QDir::separator() + name);
   }
}

QByteArray PersistantData::getValue(const QString& name)
{
   QFile file(applicationFolderPath + QDir::separator() + name);
   if (file.open(QIODevice::ReadOnly))
   {
      return file.readAll();
   }
   else
      throw UnknownValueException();
}

bool PersistantData::rmValue(const QString& name)
{
   return QFile::remove(applicationFolderPath + QDir::separator() + name);
}

bool PersistantData::createApplicationFolder()
{
   if (!QDir::home().exists(applicationFolderName))
      return QDir::home().mkdir(applicationFolderName);

   return true;
}

const QString PersistantData::applicationFolderName(".aybabtu");
const QString PersistantData::applicationFolderPath(QDir::homePath() + QDir::separator() + applicationFolderName);
const QString PersistantData::tempPostfixTerm(".temp");


