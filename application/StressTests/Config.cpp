#include <Config.h>
using namespace StressTests;

#include <algorithm>

#include <QFile>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QStringList>

namespace
{
   struct ActionDefinition
   {
      Action action;
      const char* name; // Also the key in the JSON object 'action_weights'.
      int defaultWeight;
   };

   const ActionDefinition ACTION_DEFINITIONS[] = {
      { Action::CREATE_FILE, "create_file", 8 },
      { Action::CREATE_SHARED_DIRECTORY, "create_shared_directory", 3 },
      { Action::CREATE_SUB_DIRECTORY, "create_sub_directory", 5 },
      { Action::CHANGE_NICK, "change_nick", 1 },
      { Action::DOWNLOAD, "download", 5 },
      { Action::CANCEL_DOWNLOAD, "cancel_download", 1 },
      { Action::PAUSE_DOWNLOAD, "pause_download", 2 },
      { Action::MOVE_DOWNLOADS, "move_downloads", 2 },
      { Action::DELETE_ENTRY, "delete_entry", 2 },
      { Action::JOIN_LEAVE_ROOM, "join_leave_room", 3 },
      { Action::SEND_CHAT_MESSAGE, "send_chat_message", 5 },
      { Action::RESTART_CORE, "restart_core", 1 },
   };

   void read(const QJsonObject& object, const QString& key, int& value, QStringList& errors)
   {
      if (!object.contains(key))
         return;
      const QJsonValue jsonValue = object.value(key);
      if (!jsonValue.isDouble() || jsonValue.toDouble() != static_cast<double>(jsonValue.toInt()))
         errors << QString("'%1' must be an integer").arg(key);
      else
         value = jsonValue.toInt();
   }

   void read(const QJsonObject& object, const QString& key, double& value, QStringList& errors)
   {
      if (!object.contains(key))
         return;
      const QJsonValue jsonValue = object.value(key);
      if (!jsonValue.isDouble())
         errors << QString("'%1' must be a number").arg(key);
      else
         value = jsonValue.toDouble();
   }

   void read(const QJsonObject& object, const QString& key, QString& value, QStringList& errors)
   {
      if (!object.contains(key))
         return;
      const QJsonValue jsonValue = object.value(key);
      if (!jsonValue.isString())
         errors << QString("'%1' must be a string").arg(key);
      else
         value = jsonValue.toString();
   }
}

QString StressTests::actionName(Action action)
{
   for (const auto& definition : ACTION_DEFINITIONS)
      if (definition.action == action)
         return definition.name;
   return "unknown";
}

QList<Action> StressTests::allActions()
{
   QList<Action> actions;
   for (const auto& definition : ACTION_DEFINITIONS)
      actions << definition.action;
   return actions;
}

Config::Config()
{
   for (const auto& definition : ACTION_DEFINITIONS)
      this->actionWeights.insert(definition.action, definition.defaultWeight);
}

QString Config::load(const QString& filepath)
{
   QFile file(filepath);
   if (!file.open(QIODevice::ReadOnly))
      return QString("Unable to open the configuration file '%1': %2").arg(filepath, file.errorString());

   QJsonParseError parseError;
   const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parseError);
   if (parseError.error != QJsonParseError::NoError)
      return QString("Unable to parse the configuration file '%1': %2 (offset %3)").arg(filepath, parseError.errorString()).arg(parseError.offset);
   if (!document.isObject())
      return QString("The configuration file '%1' must contain a JSON object").arg(filepath);

   const QJsonObject object = document.object();
   QStringList errors;

   read(object, "number_of_cores", this->numberOfCores, errors);
   read(object, "duration_minutes", this->durationMinutes, errors);
   read(object, "tick_min_ms", this->tickMinMs, errors);
   read(object, "tick_max_ms", this->tickMaxMs, errors);
   read(object, "file_size_mean_mb", this->fileSizeMeanMB, errors);
   read(object, "file_size_std_dev_mb", this->fileSizeStdDevMB, errors);
   read(object, "max_file_size_mb", this->maxFileSizeMB, errors);
   read(object, "max_total_size_gb", this->maxTotalSizeGB, errors);
   read(object, "remote_control_base_port", this->remoteControlBasePort, errors);
   read(object, "unicast_base_port", this->unicastBasePort, errors);
   read(object, "unicast_port_step", this->unicastPortStep, errors);
   read(object, "channel", this->channel, errors);
   read(object, "multicast_port", this->multicastPort, errors);
   read(object, "number_of_rooms", this->numberOfRooms, errors);
   read(object, "core_stop_timeout_s", this->coreStopTimeoutS, errors);
   read(object, "core_executable", this->coreExecutable, errors);

   if (object.contains("seed"))
   {
      // A string is also accepted because a JSON number can't hold every 64 bits integer.
      const QJsonValue seedValue = object.value("seed");
      bool ok = false;
      if (seedValue.isString())
         this->seed = seedValue.toString().toULongLong(&ok);
      else if (seedValue.isDouble() && seedValue.toDouble() >= 0)
      {
         this->seed = static_cast<quint64>(seedValue.toDouble());
         ok = true;
      }
      if (!ok)
         errors << "'seed' must be a positive integer or a string containing a positive integer";
   }

   if (object.contains("action_weights"))
   {
      if (!object.value("action_weights").isObject())
      {
         errors << "'action_weights' must be an object";
      }
      else
      {
         const QJsonObject weights = object.value("action_weights").toObject();
         for (auto i = weights.begin(); i != weights.end(); ++i)
         {
            const auto definition = std::find_if(std::begin(ACTION_DEFINITIONS), std::end(ACTION_DEFINITIONS), [&](const ActionDefinition& d) { return i.key() == d.name; });
            if (definition == std::end(ACTION_DEFINITIONS))
               errors << QString("Unknown action in 'action_weights': '%1'").arg(i.key());
            else
            {
               int weight = this->actionWeights[definition->action];
               read(weights, i.key(), weight, errors);
               this->actionWeights[definition->action] = weight;
            }
         }
      }
   }

   // Validation.
   if (this->numberOfCores < 1)
      errors << "'number_of_cores' must be at least 1";
   if (this->durationMinutes <= 0)
      errors << "'duration_minutes' must be positive";
   if (this->tickMinMs < 0 || this->tickMaxMs < this->tickMinMs)
      errors << "'tick_min_ms' must be positive and lesser or equal to 'tick_max_ms'";
   if (this->fileSizeMeanMB < 0 || this->fileSizeStdDevMB < 0 || this->maxFileSizeMB < 0)
      errors << "'file_size_mean_mb', 'file_size_std_dev_mb' and 'max_file_size_mb' must be positive";
   if (this->maxTotalSizeGB <= 0)
      errors << "'max_total_size_gb' must be positive";
   if (this->remoteControlBasePort < 1 || this->remoteControlBasePort + this->numberOfCores - 1 > 65535)
      errors << "'remote_control_base_port' must be in [1, 65535] for each Core";
   if (this->unicastPortStep < 1 || this->unicastBasePort < 1 || this->unicastBasePort + this->unicastPortStep * (this->numberOfCores - 1) > 65535)
      errors << "'unicast_base_port' must be in [1, 65535] for each Core and 'unicast_port_step' must be at least 1";
   if (this->channel.isEmpty())
      errors << "'channel' must not be empty";
   if (this->multicastPort < 1 || this->multicastPort > 65535)
      errors << "'multicast_port' must be in [1, 65535]";
   if (this->numberOfRooms < 1)
      errors << "'number_of_rooms' must be at least 1";
   if (this->coreStopTimeoutS < 1)
      errors << "'core_stop_timeout_s' must be at least 1";

   int totalWeight = 0;
   for (auto i = this->actionWeights.cbegin(); i != this->actionWeights.cend(); ++i)
   {
      if (i.value() < 0 || i.value() > 10)
         errors << QString("The weight of '%1' must be in [0, 10]").arg(actionName(i.key()));
      totalWeight += i.value();
   }
   if (totalWeight == 0)
      errors << "At least one action must have a weight greater than 0";

   if (!errors.isEmpty())
      return QString("Invalid configuration file '%1':\n - %2").arg(filepath, errors.join("\n - "));

   return QString();
}

QString Config::toString() const
{
   QStringList weights;
   for (auto i = this->actionWeights.cbegin(); i != this->actionWeights.cend(); ++i)
      weights << QString("%1=%2").arg(actionName(i.key())).arg(i.value());

   return QString(
      "number_of_cores=%1, duration_minutes=%2, tick=[%3, %4] ms, file size: mean=%5 MB std_dev=%6 MB max=%7 MB, "
      "max_total_size=%8 GB, remote_control_base_port=%9, unicast_base_port=%10 (step %11), channel='%12', multicast_port=%13, "
      "number_of_rooms=%14, core_stop_timeout=%15 s, core_executable='%16', action_weights: %17"
   )
      .arg(this->numberOfCores)
      .arg(this->durationMinutes)
      .arg(this->tickMinMs)
      .arg(this->tickMaxMs)
      .arg(this->fileSizeMeanMB)
      .arg(this->fileSizeStdDevMB)
      .arg(this->maxFileSizeMB)
      .arg(this->maxTotalSizeGB)
      .arg(this->remoteControlBasePort)
      .arg(this->unicastBasePort)
      .arg(this->unicastPortStep)
      .arg(this->channel)
      .arg(this->multicastPort)
      .arg(this->numberOfRooms)
      .arg(this->coreStopTimeoutS)
      .arg(this->coreExecutable)
      .arg(weights.join(", "));
}
