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

#include <Common/Settings.h>
using namespace Common;

#include <QMutexLocker>
#include <QTextStream>

#include <Protos/common.pb.h>

#include <Common/PersistentData.h>
#include <ProtoHelper.h>

/**
  * @class Common::Settings
  *
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
   Q_ASSERT(!filename.isEmpty());

   this->filename = filename;
}

/**
  * Define the settings structure.
  * The object will be deleted by this settings class.
  */
void Settings::setSettingsMessage(google::protobuf::Message* settings)
{
   Q_ASSERT(settings);

   if (this->settings)
      delete this->settings;

   this->settings = settings;
   this->descriptor = this->settings->GetDescriptor();

   this->setDefaultValues();
}

/**
  * @exception PersistentDataIOException see the class 'PersistentData'.
  */
void Settings::save() const
{
   this->saveTo(this->filename);
}

void Settings::saveTo(const QString& filename) const
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   PersistentData::setValue(filename, *this->settings, Common::Global::ROAMING, true);
}

bool Settings::load()
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(this->settings);

   if (!this->settings)
      return false;

   try
   {
      PersistentData::getValue(this->filename, *this->settings, Common::Global::ROAMING, true);
      return true;
   }
   catch (UnknownValueException&)
   {
      return false;
   }
   catch (PersistentDataIOException&)
   {
      return false;
   }
}

void Settings::remove()
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   PersistentData::rmValue(this->filename, Common::Global::ROAMING);
}

void Settings::free()
{
   if (Settings::instance)
   {
      delete Settings::instance;
      Settings::instance = 0;
   }
}

bool Settings::isSet(const QString& name) const
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return false;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
      return false;

   return this->settings->GetReflection()->HasField(*this->settings, fieldDescriptor);
}

void Settings::set(const QString& name, quint32 value)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

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

void Settings::set(const QString& name, bool value)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_BOOL)
   {
      printErrorBadType(fieldDescriptor, "bool");
      return;
   }

   this->settings->GetReflection()->SetBool(this->settings, fieldDescriptor, value);
}

void Settings::set(const QString& name, double value)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

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
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

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

void Settings::set(const QString& name, const QByteArray& value)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_BYTES)
   {
      printErrorBadType(fieldDescriptor, "bytes");
      return;
   }

   std::string valueStr;
   valueStr.assign(value.constData(), value.size());
   this->settings->GetReflection()->SetString(this->settings, fieldDescriptor, valueStr);
}

void Settings::set(const QString& name, const Hash& hash)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

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

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, quint32& value) const
{
   Q_ASSERT(fieldDescriptor);
   value = this->settings->GetReflection()->GetUInt32(*this->settings, fieldDescriptor);
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, bool& value) const
{
   Q_ASSERT(fieldDescriptor);
   value = this->settings->GetReflection()->GetBool(*this->settings, fieldDescriptor);
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, double& value) const
{
   Q_ASSERT(fieldDescriptor);
   value = this->settings->GetReflection()->GetDouble(*this->settings, fieldDescriptor);
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, QString& value) const
{
   Q_ASSERT(fieldDescriptor);
   value = QString::fromUtf8(this->settings->GetReflection()->GetString(*this->settings, fieldDescriptor).data());
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, QByteArray& value) const
{
   Q_ASSERT(fieldDescriptor);
   std::string valueStr = this->settings->GetReflection()->GetString(*this->settings, fieldDescriptor);
   value = QByteArray::fromRawData(valueStr.data(), valueStr.size());
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, Hash& hash) const
{
   Q_ASSERT(fieldDescriptor);
   hash = static_cast<const Protos::Common::Hash&>(this->settings->GetReflection()->GetMessage(*this->settings, fieldDescriptor)).hash();
}

void Settings::setDefaultValues()
{
   // Very ugly : to force the optional field to be written. TODO : find a another way.
   for (int i = 0; i < this->descriptor->field_count(); i++)
   {
      const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->field(i);

      // It doesn't work, an optional field with a default value is defined as non-set (HasField returns 'false').
      /*if (!this->settings->GetReflection()->HasField(*this->settings, fieldDescriptor))
         continue;*/

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

void Settings::rm(const QString& name)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

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

void Settings::rmAll()
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   /* ClearField doesn't work, because if we save the settings the fields will not be written in it.
   for (int i = 0; i < this->descriptor->field_count(); i++)
   {
      const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->field(i);
      this->settings->GetReflection()->ClearField(this->settings, fieldDescriptor);
   }*/

   this->setDefaultValues();
}

void Settings::printErrorNameNotFound(const QString& name)
{
   QTextStream(stderr) << QString("Settings : name \"%1\" doesn't exist").arg(name) << endl;
}

void Settings::printErrorBadType(const google::protobuf::FieldDescriptor* field, const QString& excepted)
{
   QTextStream(stderr) << QString("Settings : bad type, field name = \"%1\", expected type : \"%2\"").arg(ProtoHelper::getStr(*field, &google::protobuf::FieldDescriptor::name)).arg(excepted) << endl;
}
