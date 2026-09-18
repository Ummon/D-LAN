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
#include <utility>

#include <Protos/common.pb.h>

#include <Common/PersistentData.h>
#include <ProtoHelper.h>
#include <SharedEntry.h>

/**
  * @class Common::Settings
  *
  * Store some settings into a file via PersistentData.
  * Singleton.
  */

Settings* Settings::instance(nullptr);
QMutex Settings::instanceMutex;

Settings& Settings::getInstance()
{
   // Locked: 'SETTINGS' can be reached from several threads and two of them could otherwise build two instances.
   QMutexLocker locker(&Settings::instanceMutex);

   if (!Settings::instance)
      Settings::instance = new Settings();
   return *Settings::instance;
}

Settings::Settings() :
   filename("settings.json"), // The default name.
   settings(nullptr)
{
}

Settings::~Settings()
{
   if (this->settings)
      delete this->settings;
}

void Settings::setFilename(const QString& filename)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!filename.isEmpty());

   this->filename = filename;
}

/**
  * Define the settings structure.
  * The settings may contain default values.
  * The object will be deleted by this settings class.
  */
void Settings::setSettingsMessage(google::protobuf::Message* settings)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(settings);

   QHash<QString, const google::protobuf::FieldDescriptor*> fields;
   const auto* descriptor = settings->GetDescriptor();
   fields.reserve(descriptor->field_count());
   for (int i = 0; i < descriptor->field_count(); ++i)
   {
      const auto* field = descriptor->field(i);
      const auto& name = field->name();
      fields.insert(QString::fromUtf8(name.data(), static_cast<qsizetype>(name.size())), field);
   }

   if (this->settings)
      delete this->settings;

   this->settings = settings;
   this->fields = std::move(fields);
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
      PersistentData::setValue(filename, *this->settings, Common::Global::DataFolderType::ROAMING, true);
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
      PersistentData::getValue(this->filename, *this->settings, Common::Global::DataFolderType::ROAMING, true);
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

   PersistentData::rmValue(this->filename, Common::Global::DataFolderType::ROAMING);
}

bool Settings::saveToACustomDirectory(const QString& directory) const
{
   QMutexLocker locker(&this->mutex);
   Q_ASSERT(this->settings);
   if (!this->settings)
      return false;

   try
   {
      PersistentData::setValue(directory, filename, *this->settings, Common::Global::DataFolderType::ROAMING, true);
      return true;
   }
   catch (PersistentDataIOException&)
   {
      return false;
   }
}

bool Settings::loadFromACustomDirectory(const QString& directory)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(this->settings);

   if (!this->settings)
      return false;

   try
   {
      PersistentData::getValue(directory, this->filename, *this->settings, Common::Global::DataFolderType::ROAMING, true);
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

/**
  * @remarks Must only be called during the shutdown, when no other thread uses the settings anymore.
  */
void Settings::free()
{
   QMutexLocker locker(&Settings::instanceMutex);

   delete Settings::instance;
   Settings::instance = nullptr;
}

bool Settings::isSet(const QString& name) const
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return false;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }

   if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_ENUM)
   {
      const google::protobuf::EnumDescriptor* enumDescriptor = fieldDescriptor->enum_type();
      const google::protobuf::EnumValueDescriptor* enumValue = enumDescriptor->FindValueByNumber(value);
      if (!enumValue)
      {
         printError(QString("Settings: the value %1 doesn't belong to the enumeration of the field \"%2\"").arg(value).arg(name));
         return;
      }
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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
   this->settings->GetReflection()->SetString(this->settings, fieldDescriptor, value.toStdString());
}

void Settings::set(const QString& name, const QByteArray& value)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (
      fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_MESSAGE ||
      fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_MESSAGE &&
      fieldDescriptor->message_type()->name() != "Language"
   )
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   if (
      fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_MESSAGE ||
      fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_MESSAGE &&
      fieldDescriptor->message_type()->full_name() != message.GetTypeName()
   )
   {
      printErrorBadType(fieldDescriptor, QString::fromUtf8(message.GetTypeName().data()));
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

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

void Settings::set(const QString& name, const QList<QString>& values)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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

   if (fieldDescriptor->type() != google::protobuf::FieldDescriptor::TYPE_STRING)
   {
      printErrorBadType(fieldDescriptor, "string");
      return;
   }

   for (QListIterator<QString> i(values); i.hasNext();)
      this->settings->GetReflection()->AddString(this->settings, fieldDescriptor, i.next().toStdString());
}

void Settings::set(const QString& name, int index, quint32 value)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
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
      value = this->settings->GetReflection()->GetEnumValue(*this->settings, fieldDescriptor);
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
   std::string scratch;
   const auto& valueStr = this->settings->GetReflection()->GetStringReference(*this->settings, fieldDescriptor, &scratch);
   value = QString::fromStdString(valueStr); // Preserve embedded null characters.
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, QByteArray& value) const
{
   Q_ASSERT(fieldDescriptor);
   std::string scratch;
   const auto& valueStr = this->settings->GetReflection()->GetStringReference(*this->settings, fieldDescriptor, &scratch);
   value = QByteArray(valueStr.data(), static_cast<qsizetype>(valueStr.size()));
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, Hash& hash) const
{
   Q_ASSERT(fieldDescriptor);
   hash = static_cast<const Protos::Common::Hash&>(this->settings->GetReflection()->GetMessage(*this->settings, fieldDescriptor)).hash();
}

void Settings::get(const google::protobuf::FieldDescriptor* fieldDescriptor, QLocale& lang) const
{
   Q_ASSERT(fieldDescriptor);

   const auto& langMess = static_cast<const Protos::Common::Language&>(this->settings->GetReflection()->GetMessage(*this->settings, fieldDescriptor));

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
   const auto* reflection = this->settings->GetReflection();
   const int count = reflection->FieldSize(*this->settings, fieldDescriptor);
   values.reserve(count);

   if (fieldDescriptor->type() == google::protobuf::FieldDescriptor::TYPE_ENUM)
   {
      for (int i = 0; i < count; ++i)
         values << reflection->GetRepeatedEnumValue(*this->settings, fieldDescriptor, i);
   }
   else
   {
      for (int i = 0; i < count; ++i)
         values << reflection->GetRepeatedUInt32(*this->settings, fieldDescriptor, i);
   }
}

void Settings::getRepeated(const google::protobuf::FieldDescriptor* fieldDescriptor, QList<QString>& values) const
{
   Q_ASSERT(fieldDescriptor);

   const auto* reflection = this->settings->GetReflection();
   const int count = reflection->FieldSize(*this->settings, fieldDescriptor);
   values.reserve(count);
   std::string scratch;
   for (int i = 0; i < count; ++i)
      values << QString::fromStdString(reflection->GetRepeatedStringReference(*this->settings, fieldDescriptor, i, &scratch));
}

void Settings::getRepeated(const google::protobuf::FieldDescriptor* fieldDescriptor, QList<Protos::Common::SharedEntry>& values) const
{
   Q_ASSERT(fieldDescriptor);

   const auto entries = this->settings->GetReflection()->GetRepeatedFieldRef<Protos::Common::SharedEntry>(*this->settings, fieldDescriptor);
   values.reserve(entries.size());
   for (const auto& entry : entries)
      values << entry;
}

void Settings::rm(const QString& name)
{
   QMutexLocker locker(&this->mutex);

   Q_ASSERT(!name.isEmpty());
   Q_ASSERT(this->settings);

   if (!this->settings)
      return;

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->fields.value(name, nullptr);
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return;
   }
   this->settings->GetReflection()->ClearField(this->settings, fieldDescriptor);
}

void Settings::printError(const QString& name)
{
   QTextStream(stderr) << name << Qt::endl;
}

void Settings::printErrorNameNotFound(const QString& name)
{
   QTextStream(stderr) << QString("Settings: name \"%1\" doesn't exist").arg(name) << Qt::endl;
}

void Settings::printErrorBadType(const google::protobuf::FieldDescriptor* field, const QString& excepted)
{
   // ProtoHelper::getStr(*field, &google::protobuf::FieldDescriptor::name)
   QTextStream(stderr) << QString("Settings: bad type, field name = \"%1\", expected type: \"%2\"").arg(field->name()).arg(excepted) << Qt::endl;
}
