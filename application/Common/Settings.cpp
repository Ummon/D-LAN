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

bool Settings::save() const
{
   return this->saveTo(this->filename);
}

bool Settings::saveTo(const QString& filename) const
{
   QMutexLocker locker(&this->mutex);
   Q_ASSERT(this->settings);
   if (!this->settings)
      return false;

   try
   {
      PersistentData::setValue(filename, *this->settings, Common::Global::ROAMING, true);
      return true;
   }
   catch (PersistentDataIOException&)
   {
      return false;
   }
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

bool Settings::saveToACutomDirectory(const QString& directory) const
{
   QMutexLocker locker(&this->mutex);
   Q_ASSERT(this->settings);
   if (!this->settings)
      return false;

   try
   {
      PersistentData::setValue(directory, filename, *this->settings, Common::Global::ROAMING, true);
      return true;
   }
   catch (PersistentDataIOException&)
   {
      return false;
   }
}

bool Settings::loadFromACutomDirectory(const QString& directory)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(this->settings);

   if (!this->settings)
      return false;

   try
   {
      PersistentData::getValue(directory, this->filename, *this->settings, Common::Global::ROAMING, true);
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

   if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_ENUM)
   {
      const google::protobuf::EnumDescriptor* enumDescriptor = fieldDescriptor->enum_type();
      const google::protobuf::EnumValueDescriptor* enumValue = enumDescriptor->FindValueByNumber(value);
      this->settings->GetReflection()->SetEnum(this->settings, fieldDescriptor, enumValue);
   }
   else if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_UINT32)
   {
      this->settings->GetReflection()->SetUInt32(this->settings, fieldDescriptor, value);
   }
   else
   {
      printErrorBadType(fieldDescriptor, "uint32");
      return;
   }
}

void Settings::set(const QString& name, quint64 value)
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
   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_UINT64)
   {
      printErrorBadType(fieldDescriptor, "uint64");
      return;
   }

   this->settings->GetReflection()->SetUInt64(this->settings, fieldDescriptor, value);
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

void Settings::set(const QString& name, const QLocale& lang)
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
       fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_MESSAGE && fieldDescriptor->message_type()->name() != "Language")
   {
      printErrorBadType(fieldDescriptor, "Language");
      return;
   }

   Protos::Common::Language language;
   ProtoHelper::setLang(language, lang);
   this->settings->GetReflection()->MutableMessage(this->settings, fieldDescriptor)->CopyFrom(language);
}

void Settings::set(const QString& name, const google::protobuf::Message& message)
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
       fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_MESSAGE && fieldDescriptor->message_type()->full_name() != message.GetTypeName())
   {
      printErrorBadType(fieldDescriptor, QString::fromStdString(message.GetTypeName()));
      return;
   }

   this->settings->GetReflection()->MutableMessage(this->settings, fieldDescriptor)->CopyFrom(message);
}

void Settings::set(const QString& name, const QList<quint32>& values)
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

   if (!fieldDescriptor->is_repeated())
   {
      printError(QString("The field '%1' isn't a repeated field").arg(name));
      return;
   }

   this->settings->GetReflection()->ClearField(this->settings, fieldDescriptor);

   if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_ENUM)
   {
      const google::protobuf::EnumDescriptor* enumDescriptor = fieldDescriptor->enum_type();
      for (QListIterator<quint32> i(values); i.hasNext();)
      {
         const google::protobuf::EnumValueDescriptor* enumValue = enumDescriptor->FindValueByNumber(i.next());
         if (enumValue)
            this->settings->GetReflection()->AddEnum(this->settings, fieldDescriptor, enumValue);
      }
   }
   else
   {
      for (QListIterator<quint32> i(values); i.hasNext();)
         this->settings->GetReflection()->AddUInt32(this->settings, fieldDescriptor, i.next());
   }
}

void Settings::set(const QString& name, int index, quint32 value)
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

   if (!fieldDescriptor->is_repeated())
   {
      printError(QString("The field '%1' isn't a repeated field").arg(name));
      return;
   }

   if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_ENUM)
   {
      const google::protobuf::EnumDescriptor* enumDescriptor = fieldDescriptor->enum_type();
      const google::protobuf::EnumValueDescriptor* enumValue = enumDescriptor->FindValueByNumber(value);
      while (this->settings->GetReflection()->FieldSize(*this->settings, fieldDescriptor) <= index)
         this->settings->GetReflection()->AddEnum(this->settings, fieldDescriptor, enumDescriptor->value(0));
      if (enumValue)
         this->settings->GetReflection()->SetRepeatedEnum(this->settings, fieldDescriptor, index, enumValue);
   }
   else
   {
      while (this->settings->GetReflection()->FieldSize(*this->settings, fieldDescriptor) <= index)
         this->settings->GetReflection()->AddUInt32(this->settings, fieldDescriptor, 0);
      this->settings->GetReflection()->SetRepeatedUInt32(this->settings, fieldDescriptor, index, value);
   }
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, quint32& value) const
{
   Q_ASSERT(fieldDescriptor);
   if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_ENUM)
   {
      value = this->settings->GetReflection()->GetEnum(*this->settings, fieldDescriptor)->number();
   }
   else
   {
      value = this->settings->GetReflection()->GetUInt32(*this->settings, fieldDescriptor);
   }
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, quint64& value) const
{
   Q_ASSERT(fieldDescriptor);
   value = this->settings->GetReflection()->GetUInt64(*this->settings, fieldDescriptor);
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

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, QLocale& lang) const
{
   Q_ASSERT(fieldDescriptor);

   Protos::Common::Language langMess = static_cast<const Protos::Common::Language&>(this->settings->GetReflection()->GetMessage(*this->settings, fieldDescriptor));

   lang = ProtoHelper::getLang(langMess);
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, google::protobuf::Message& message) const
{
   Q_ASSERT(fieldDescriptor);

   message.CopyFrom(this->settings->GetReflection()->GetMessage(*this->settings, fieldDescriptor));
}

void Settings::getRepeated(const google::protobuf::FieldDescriptor* fieldDescriptor, QList<quint32>& values) const
{
   Q_ASSERT(fieldDescriptor);

   if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_ENUM)
   {
      for (int i = 0; i < this->settings->GetReflection()->FieldSize(*this->settings, fieldDescriptor); i++)
         values << this->settings->GetReflection()->GetRepeatedEnum(*this->settings, fieldDescriptor, i)->number();
   }
   else
   {
      for (int i = 0; i < this->settings->GetReflection()->FieldSize(*this->settings, fieldDescriptor); i++)
         values << this->settings->GetReflection()->GetRepeatedUInt32(*this->settings, fieldDescriptor, i);
   }
}

void Settings::setDefaultValues()
{
   // Very ugly : to force the optional field to be written. TODO: find a another way.
   for (int i = 0; i < this->descriptor->field_count(); i++)
   {
      const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->field(i);

      // It doesn't work, an optional field with a default value is defined as non-set (HasField returns 'false').
      /*if (!this->settings->GetReflection()->HasField(*this->settings, fieldDescriptor))
         continue;*/

      if (fieldDescriptor->is_repeated())
         this->settings->GetReflection()->ClearField(this->settings, fieldDescriptor);
      else
         switch (fieldDescriptor->type())
         {
         case google::protobuf::FieldDescriptor::TYPE_UINT32:
            this->settings->GetReflection()->SetUInt32(this->settings, fieldDescriptor, fieldDescriptor->default_value_uint32());
            break;
         case google::protobuf::FieldDescriptor::TYPE_ENUM:
            this->settings->GetReflection()->SetEnum(this->settings, fieldDescriptor, fieldDescriptor->default_value_enum());
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

void Settings::printError(const QString& name)
{
   QTextStream(stderr) << name << endl;
}

void Settings::printErrorNameNotFound(const QString& name)
{
   QTextStream(stderr) << QString("Settings : name \"%1\" doesn't exist").arg(name) << endl;
}

void Settings::printErrorBadType(const google::protobuf::FieldDescriptor* field, const QString& excepted)
{
   QTextStream(stderr) << QString("Settings : bad type, field name = \"%1\", expected type : \"%2\"").arg(ProtoHelper::getStr(*field, &google::protobuf::FieldDescriptor::name)).arg(excepted) << endl;
}
