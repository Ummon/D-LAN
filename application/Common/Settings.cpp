#include <Common/Settings.h>
using namespace Common;

#include <QMutexLocker>
#include <QTextStream>

#include <google/protobuf/message.h>

#include <Common/PersistantData.h>

/**
  * @class Settings
  * Store some settings into a file via PersistantData.
  * Singleton.
  */

const QString Settings::FILENAME("settings.txt");
Settings* Settings::instance(0);

Settings& Settings::getInstance()
{
   if (!Settings::instance)
      Settings::instance = new Settings();
   return *Settings::instance;
}

Settings::Settings()
   : persisted(false), mutex(QMutex::Recursive)
{
   this->descriptor = this->settings.GetDescriptor();
   this->load();
}

bool Settings::arePersisted()
{
   QMutexLocker lock(&this->mutex);
   return this->persisted;
}

void Settings::save()
{
   QMutexLocker lock(&this->mutex);
   PersistantData::setValue(FILENAME, this->settings, true);
   this->persisted = true;
}

void Settings::load()
{
   QMutexLocker lock(&this->mutex);
   try
   {
      PersistantData::getValue(FILENAME, this->settings, true);
      this->persisted = true;
   }
   catch (Common::UnknownValueException&)
   {
   }
}

void Settings::remove()
{
   QMutexLocker lock(&this->mutex);
   PersistantData::rmValue(FILENAME);
   this->persisted = false;
}

void Settings::set(const QString& name, quint32 value)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_UINT32)
   {
      printErrorBadType(QString::fromStdString(fieldDescriptor->name()), "uint32");
      return;
   }

   this->settings.GetReflection()->SetUInt32(&this->settings, fieldDescriptor, value);
}

void Settings::set(const QString& name, double value)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_DOUBLE)
   {
      printErrorBadType(QString::fromStdString(fieldDescriptor->name()), "double");
      return;
   }

   this->settings.GetReflection()->SetDouble(&this->settings, fieldDescriptor, value);
}

void Settings::set(const QString& name, const QString& value)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_STRING)
   {
      printErrorBadType(QString::fromStdString(fieldDescriptor->name()), "string");
      return;
   }
   this->settings.GetReflection()->SetString(&this->settings, fieldDescriptor, value.toStdString());
}

void Settings::set(const QString& name, const Hash& hash)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_MESSAGE ||
       fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_MESSAGE && fieldDescriptor->message_type()->name() != "Hash")
   {
      printErrorBadType(QString::fromStdString(fieldDescriptor->name()), "Hash");
      return;
   }

   Protos::Common::Hash hashMessage;
   hashMessage.set_hash(hash.getData(), Hash::HASH_SIZE);
   this->settings.GetReflection()->MutableMessage(&this->settings, fieldDescriptor)->CopyFrom(hashMessage);
}

void Settings::get(const QString& name, quint32& value)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   value = this->settings.GetReflection()->GetUInt32(this->settings, fieldDescriptor);
}

void Settings::get(const QString& name, double& value)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   value = this->settings.GetReflection()->GetDouble(this->settings, fieldDescriptor);
}

void Settings::get(const QString& name, QString& value)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   value = QString::fromStdString(this->settings.GetReflection()->GetString(this->settings, fieldDescriptor));
}

void Settings::get(const QString& name, Hash& hash)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   hash = static_cast<const Protos::Common::Hash&>(this->settings.GetReflection()->GetMessage(this->settings, fieldDescriptor)).hash().data();
}

void Settings::rm(const QString& name)
{
   QMutexLocker lock(&this->mutex);
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   this->settings.GetReflection()->ClearField(&this->settings, fieldDescriptor);
}

void Settings::printErrorNameNotFound(const QString& name)
{
   QTextStream(stderr) << QString("Settings : name \"%1\" doesn't exist").arg(name) << endl;
}

void Settings::printErrorBadType(const QString& fieldName, const QString& excepted)
{
   QTextStream(stderr) << QString("Settings : bad type, field name = \"%1\", expected type : \"%2\"").arg(fieldName).arg(excepted) << endl;
}
