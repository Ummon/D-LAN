#include <Common/PersistantData.h>
using namespace Common;

#include <fstream>
using namespace std;

#include <QDir>
#include <QtDebug>

#include <google/protobuf/text_format.h>
#if DEBUG
#  include <google/protobuf/io/zero_copy_stream_impl.h>
#endif

#include <Constants.h>
#include <Global.h>

/**
  * @class PersistantData
  * Some little functions to persist data and retrieve it.
  * The data are persisted in the user directory.
  * The data are described by a Protocol Buffer message.
  * Theses functions can be used for the application settings.
  */

const QString PersistantData::TEMP_SUFFIX_TERM(".temp");

/**
  * Define a value associated to a name.
  * You may refer to the name policy of the platform. Try to avoir special characters or space.
  * You can use an extension in the name like "settings.conf".
  */
void PersistantData::setValue(const QString& name, const google::protobuf::Message& data, bool humanReadable)
{
   if (Global::createApplicationFolder())
   {
      QString tempName(name + TEMP_SUFFIX_TERM);

      {
         ofstream file((APPLICATION_FOLDER_PATH + '/' + tempName).toStdString().data(), ios_base::binary | ios_base::out);
         // if (file.fail()) // TODO : check if failure
#if !DEBUG
         if (humanReadable)
         {
#endif
            google::protobuf::io::OstreamOutputStream zeroCopyStream(&file);
            google::protobuf::TextFormat::Print(data, &zeroCopyStream);
#if !DEBUG
         }
         else
         {
            data.SerializeToOstream(&file);
         }
#endif
      }

      // TODO : Some data loss can occure here, we must remove the file first
      // because 'rename' cannot overwrite an existing file.
      // Using a system primitive can help!?
      QFile::remove(APPLICATION_FOLDER_PATH + '/' + name);
      QFile::rename(APPLICATION_FOLDER_PATH + '/' + tempName, APPLICATION_FOLDER_PATH + QDir::separator() + name);
   }
}

/**
  * Retrieve the data associated to a given name.
  * @exception UnknownValueException Throwed if the value doesn't exist
  */
void PersistantData::getValue(const QString& name, google::protobuf::Message& data, bool humanReadable)
{
   ifstream file((APPLICATION_FOLDER_PATH + '/' + name).toStdString().data(), ios_base::binary | ios_base::in);
   if (file.fail())
      throw UnknownValueException();

#if !DEBUG
   if (humanReadable)
   {
#endif
      google::protobuf::io::IstreamInputStream zeroCopyStream(&file);
      google::protobuf::TextFormat::Parse(&zeroCopyStream, &data);
#if !DEBUG
   }
   else
   {
      data.ParseFromIstream(&file);
   }
#endif
}

/**
  * Remove a data.
  * @return Return false if the data didn't exist.
  */
bool PersistantData::rmValue(const QString& name)
{
   return QFile::remove(APPLICATION_FOLDER_PATH + '/' + name);
}
