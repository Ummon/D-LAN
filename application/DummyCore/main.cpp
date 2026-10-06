#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QTextStream>

#include <google/protobuf/stubs/common.h>

#include <Common/Constants.h>

#include <Server.h>
#include <State.h>

using namespace DummyCore;

namespace
{
   const QString STATE_FILENAME("DummyCore.json");

   void printUsage(const QString& appName)
   {
      QTextStream out(stdout);
      out << "Usage: " << appName << " [--port <remote control port>] [<state file>]" << Qt::endl
          << "  A Core without any logic: it always gives the same state to the connected GUI, for example to make screenshots." << Qt::endl
          << "  It doesn't share anything and it isn't seen by the other peers. Only a GUI running on this computer can connect to it." << Qt::endl
          << "  --port <remote control port>: The port the GUI connects to, by default the one of the Core: " << Common::Constants::DEFAULT_CORE_REMOTE_CONTROL_PORT << "." << Qt::endl
          << "  <state file>: A JSON file, by default '" << STATE_FILENAME << "' next to the executable." << Qt::endl
          << "    It defines the peers and their files, the result of any search, the downloads and the uploads." << Qt::endl
          << "    See 'DummyCore.example.json' and its variants per language, for example 'DummyCore.example.fr.json'." << Qt::endl
          << "  The GUI must be launched with '--no-auto-start', otherwise it launches the real Core before connecting to it." << Qt::endl
          << "  Press Ctrl-C to stop." << Qt::endl;
   }
}

int main(int argc, char* argv[])
{
   QCoreApplication app(argc, argv);
   GOOGLE_PROTOBUF_VERIFY_VERSION;

   QTextStream out(stdout);
   QTextStream err(stderr);

   const QStringList arguments = app.arguments();
   const QString appName = QFileInfo(arguments.first()).fileName();

   quint16 port = Common::Constants::DEFAULT_CORE_REMOTE_CONTROL_PORT;
   QString statePath;
   for (int i = 1; i < arguments.size(); i++)
   {
      const QString& arg = arguments[i];
      if (arg == "-h" || arg == "--help")
      {
         printUsage(appName);
         return 0;
      }
      else if (arg == "--port" && i < arguments.size() - 1)
      {
         bool ok = false;
         const uint value = arguments[++i].toUInt(&ok);
         if (!ok || value == 0 || value > 65535)
         {
            err << "Invalid remote control port: " << arguments[i] << Qt::endl;
            return 2;
         }
         port = static_cast<quint16>(value);
      }
      else if (!arg.startsWith('-') && statePath.isEmpty())
         statePath = arg;
      else
      {
         printUsage(appName);
         return 2;
      }
   }

   if (statePath.isEmpty())
      statePath = QDir(app.applicationDirPath()).absoluteFilePath(STATE_FILENAME);

   State state;
   const QString stateError = state.load(statePath);
   if (!stateError.isEmpty())
   {
      err << stateError << Qt::endl;
      return 2;
   }

   Server server(state);
   const QString listenError = server.listen(port);
   if (!listenError.isEmpty())
   {
      err << listenError << Qt::endl;
      return 2;
   }

   out << "State read from '" << statePath << "': "
       << state.getState().peers_size() << " peer(s), "
       << state.getState().downloads_size() << " download(s), "
       << state.getState().uploads_size() << " upload(s)" << Qt::endl
       << "Waiting for a GUI on port " << server.getPort() << ", launch it with '--no-auto-start'" << Qt::endl;

   return app.exec();
}
