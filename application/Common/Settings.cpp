#include <Common/Settings.h>
using namespace Common;

#include <QMutexLocker>
#include <QTextStream>

#include <Protos/common.pb.h>

#include <Common/PersistentData.h>
#include <ProtoHelper.h>

/**
  * @class Settings
  * Store some settings into a file via PersistentData.
  * Singleton.
  */

Settings* Settings::instance(0);

Settings& Settings::getInstance()
{
   if (!Settings::instance)
      Settings::instance = new Settings();
   return *Settings::instance;
}

Settings::Settings() :
   filename("settings.txt"), // The default name.
   settings(0),
   mutex(QMutex::Recursive)
{
}

Settings::~Settings()
{
   if (this->settings)
      delete this->settings;
}

void Settings::setFilename(const QString& filename)
{
   this->filename = filename;
}

/**
  * Define the settings structure.
  * The object will be deleted by this settings class.
  */
void Settings::setSettingsMessage(google::protobuf::Message* settings)
{
   if (this->settings)
      delete this->settings;

   this->settings = settings;
   this->descriptor = this->settings->GetDescriptor();

   // Very ugly : to force the optional field to be written. TODO : find a another way.
   for (int i = 0; i < this->descriptor->field_count(); i++)
   {
      const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->field(i);
      switch(fieldDescriptor->type())
      {
      case google::protobuf::FieldDescriptor::TYPE_UINT32:
         this->settings->GetReflection()->SetUInt32(this->settings, fieldDescriptor, fieldDescriptor->default_value_uint32());
         break;
      case google::protobuf::FieldDescriptor::TYPE_DOUBLE:
         this->settings->GetReflection()->SetDouble(this->settings, fieldDescriptor, fieldDescriptor->default_value_double());
         break;
      case google::protobuf::FieldDescriptor::TYPE_STRING:
         this->settings->GetReflection()->SetString(this->settings, fieldDescriptor, fieldDescriptor->default_value_string());
         break;
      default:
         break;
      }
   }
}

/**
  * @exception PersistentDataIOException see the class 'PersistentData'.
  */
void Settings::save() const
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   PersistentData::setValue(this->filename, *this->settings, true);
}

void Settings::load()
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   try
   {
      PersistentData::getValue(this->filename, *this->settings, true);
   }
   catch (Common::UnknownValueException&)
   {
   }
}

void Settings::remove()
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   PersistentData::rmValue(this->filename);
}

bool Settings::isSet(const QString& name) const
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return false;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
      return false;

   return this->settings->GetReflection()->HasField(*this->settings, fieldDescriptor);
}

void Settings::set(const QString& name, quint32 value)
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_UINT32)
   {
      printErrorBadType(fieldDescriptor, "uint32");
      return;
   }

   this->settings->GetReflection()->SetUInt32(this->settings, fieldDescriptor, value);
}

void Settings::set(const QString& name, double value)
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_DOUBLE)
   {
      printErrorBadType(fieldDescriptor, "double");
      return;
   }

   this->settings->GetReflection()->SetDouble(this->settings, fieldDescriptor, value);
}

void Settings::set(const QString& name, const QString& value)
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_STRING)
   {
      printErrorBadType(fieldDescriptor, "string");
      return;
   }
   QByteArray array = value.toUtf8();
   this->settings->GetReflection()->SetString(this->settings, fieldDescriptor, array.data());
}

void Settings::set(const QString& name, const Hash& hash)
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_MESSAGE ||
       fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_MESSAGE && fieldDescriptor->message_type()->name() != "Hash")
   {
      printErrorBadType(fieldDescriptor, "Hash");
      return;
   }

   Protos::Common::Hash hashMessage;
   hashMessage.set_hash(hash.getData(), Hash::HASH_SIZE);
   this->settings->GetReflection()->MutableMessage(this->settings, fieldDescriptor)->CopyFrom(hashMessage);
}

void Settings::get(const QString& name, quint32& value) const
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   value = this->settings->GetReflection()->GetUInt32(*this->settings, fieldDescriptor);
}

void Settings::get(const QString& name, double& value) const
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   value = this->settings->GetReflection()->GetDouble(*this->settings, fieldDescriptor);
}

void Settings::get(const QString& name, QString& value) const
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   value = QString::fromUtf8(this->settings->GetReflection()->GetString(*this->settings, fieldDescriptor).data());
}

void Settings::get(const QString& name, Hash& hash) const
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   hash = static_cast<const Protos::Common::Hash&>(this->settings->GetReflection()->GetMessage(*this->settings, fieldDescriptor)).hash().data();
}

void Settings::rm(const QString& name)
{
   QMutexLocker lock(&this->mutex);
   if (!this->settings)
      return;
   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   this->settings->GetReflection()->ClearField(this->settings, fieldDescriptor);
}

void Settings::printErrorNameNotFound(const QString& name)
{
   QTextStream(stderr) << QString("Settings : name \"%1\" doesn't exist").arg(name) << endl;
}

void Settings::printErrorBadType(const google::protobuf::FieldDescriptor* field, const QString& excepted)
{   
   QTextStream(stderr) << QString("Settings : bad type, field name = \"%1\", expected type : \"%2\"").arg(Common::ProtoHelper::getStr(*field, &google::protobuf::FieldDescriptor::name)).arg(excepted) << endl;
}
