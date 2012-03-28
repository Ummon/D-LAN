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
  
#ifndef COMMON_SETTINGS_H
#define COMMON_SETTINGS_H

#include <QString>
#include <QMutex>
#include <QLocale>

#include <google/protobuf/message.h>
#include <google/protobuf/descriptor.h>

#include <Common/Hash.h>

#define SETTINGS Common::Settings::getInstance() // Don't do this at home, kids!

namespace Common
{
   class Settings
   {
      static Settings* instance;
      Settings();

   public:
      ~Settings();
      static Settings& getInstance();

      void setFilename(const QString& filename);
      void setSettingsMessage(google::protobuf::Message* settings);

      bool save() const;
      bool saveTo(const QString& filename) const;
      bool load();
      void remove();

      bool saveToACutomDirectory(const QString& directory) const;
      bool loadFromACutomDirectory(const QString& directory);

      void free();

      bool isSet(const QString& name) const;

      void set(const QString& name, quint32 value);
      void set(const QString& name, quint64 value);
      void set(const QString& name, bool value);
      void set(const QString& name, double hash);
      void set(const QString& name, const QString& value);
      void set(const QString& name, const QByteArray& value);
      void set(const QString& name, const Hash& hash);
      void set(const QString& name, const QLocale& lang);
      void set(const QString& name, const google::protobuf::Message& message);

      void set(const QString& name, const QList<quint32>& values);
      void set(const QString& name, int index, quint32 value);

      template <typename T>
      T get(const QString& name) const;

      template <typename T>
      QList<T> getRepeated(const QString& name) const;

      void rm(const QString& name);
      void rmAll();

   private:
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, quint32& value) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, quint64& value) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, bool& value) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, double& value) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, QString& value) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, QByteArray& value) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, Hash& hash) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, QLocale& lang) const;
      void get(const google::protobuf::FieldDescriptor* fieldDescriptor, google::protobuf::Message& message) const;

      void getRepeated(const google::protobuf::FieldDescriptor* fieldDescriptor, QList<quint32>& values) const;

      void setDefaultValues();

      static void printError(const QString& error);
      static void printErrorNameNotFound(const QString& name);
      static void printErrorBadType(const google::protobuf::FieldDescriptor* field, const QString& excepted);

      QString filename; ///< The name of the file cache saved in the home directory.
      google::protobuf::Message* settings;
      mutable QMutex mutex;

      const google::protobuf::Descriptor* descriptor;
   };
}

/***** Definitions *****/
using namespace Common;


template <typename T>
T Settings::get(const QString& name) const
{
   QMutexLocker locker(&this->mutex);

   if (!this->settings)
      return T();

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return T();
   }

   T value;
   this->get(fieldDescriptor, value);
   return value;
}

template <typename T>
QList<T> Settings::getRepeated(const QString& name) const
{
   QMutexLocker locker(&this->mutex);

   if (!this->settings)
      return QList<T>();

   const google::protobuf::FieldDescriptor* fieldDescriptor = this->descriptor->FindFieldByName(name.toStdString());
   if (!fieldDescriptor)
   {
      printErrorNameNotFound(name);
      return QList<T>();
   }

   QList<T> values;
   this->getRepeated(fieldDescriptor, values);
   return values;
}

#endif
