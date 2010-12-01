#ifndef COMMON_SETTINGS_H
#define COMMON_SETTINGS_H

#include <QString>
#include <QMutex>

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

      void save() const;
      void load();
      void remove();

      bool isSet(const QString& name) const;

      void set(const QString& name, quint32 value);
      void set(const QString& name, double hash);
      void set(const QString& name, const QString& value);
      void set(const QString& name, const Hash& hash);

      void get(const QString& name, quint32& value) const;
      void get(const QString& name, double& value) const;
      void get(const QString& name, QString& value) const;
      void get(const QString& name, Hash& hash) const;

      template <typename T>
      T get(const QString& name) const;

      void rm(const QString& name);

   private:
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
   QMutexLocker lock(&this->mutex);
   T value;
   this->get(name, value);
   return value;
}

#endif
