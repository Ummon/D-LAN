#include <Connection.h>
using namespace DummyCore;

#include <QRandomGenerator64>
#include <QTextStream>

#include <Common/Network/RemoteControlAuthentication.h>

void Connection::Logger::logDebug(const QString&)
{
}

void Connection::Logger::logError(const QString& message)
{
   QTextStream(stderr) << message << Qt::endl;
}

/**
  * The ID of the dummy Core is put in the header of each message, it's how the GUI knows which peer is the Core
  * it's connected to.
  */
Connection::Connection(const State& state, QTcpSocket* socket) :
   MessageSocket(new Connection::Logger(), socket, state.getSelfID()),
   state(state)
{
   this->timerRefresh.setInterval(REFRESH_RATE);
   this->timerRefresh.setSingleShot(true);
   connect(&this->timerRefresh, &QTimer::timeout, this, &Connection::refresh);
}

/**
  * The same state is sent again and again: the GUI needs some of them to show a remaining time for the downloads.
  */
void Connection::refresh()
{
   if (this->waitForStateResult || !this->isListening())
      return;

   this->waitForStateResult = true;
   this->send(Common::MessageHeader::GUI_STATE, this->state.getState());
}

/**
  * The Core always begins. All the connections are local: no password is asked, see 'Protos.GUI.AskForAuthentication'.
  */
void Connection::onStartListening()
{
   Protos::GUI::AskForAuthentication askForAuthentication;
   askForAuthentication.set_protocol_version(Common::RemoteControlAuthentication::PROTOCOL_VERSION);
   askForAuthentication.set_salt_challenge(QRandomGenerator64::global()->generate64() | 1); // Never 0.
   this->send(Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION, askForAuthentication);
}

void Connection::onNewMessage(const Common::Message& message)
{
   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::GUI_AUTHENTICATION:
      {
         Protos::GUI::AuthenticationResult result;
         result.set_status(Protos::GUI::AuthenticationResult::AUTH_OK);
         this->send(Common::MessageHeader::GUI_AUTHENTICATION_RESULT, result);
         this->refresh();
      }
      break;

   case Common::MessageHeader::GUI_STATE_RESULT:
      this->waitForStateResult = false;
      this->timerRefresh.start();
      break;

   case Common::MessageHeader::GUI_SEARCH:
      for (Protos::Common::FindResult result : this->state.getSearchResults())
      {
         result.set_tag(message.getMessage<Protos::GUI::Search>().tag());
         this->send(Common::MessageHeader::GUI_SEARCH_RESULT, result);
      }
      break;

   case Common::MessageHeader::GUI_BROWSE:
      this->send(Common::MessageHeader::GUI_BROWSE_RESULT, this->state.browse(message.getMessage<Protos::GUI::Browse>()));
      break;

   // The GUI waits for an answer to the following ones.
   case Common::MessageHeader::GUI_CHAT_MESSAGE:
      this->send(Common::MessageHeader::GUI_CHAT_MESSAGE_RESULT, Protos::GUI::ChatMessageResult());
      break;

   case Common::MessageHeader::GUI_LOCAL_BROWSE:
      {
         Protos::GUI::LocalBrowseResult result;
         result.set_tag(message.getMessage<Protos::GUI::LocalBrowse>().tag());
         this->send(Common::MessageHeader::GUI_LOCAL_BROWSE_RESULT, result);
      }
      break;

   case Common::MessageHeader::GUI_LOCAL_BROWSE_QUICK_ACCESS:
      {
         Protos::GUI::LocalBrowseQuickAccessResult result;
         result.set_tag(message.getMessage<Protos::GUI::LocalBrowseQuickAccess>().tag());
         this->send(Common::MessageHeader::GUI_LOCAL_BROWSE_QUICK_ACCESS_RESULT, result);
      }
      break;

   // The Core sends a new state right after these commands. Here they don't change anything.
   case Common::MessageHeader::GUI_SETTINGS:
   case Common::MessageHeader::GUI_CHANGE_PASSWORD:
   case Common::MessageHeader::GUI_CANCEL_DOWNLOADS:
   case Common::MessageHeader::GUI_PAUSE_DOWNLOADS:
   case Common::MessageHeader::GUI_MOVE_DOWNLOADS:
   case Common::MessageHeader::GUI_DOWNLOAD:
   case Common::MessageHeader::GUI_REFRESH:
   case Common::MessageHeader::GUI_REFRESH_NETWORK_INTERFACES:
      this->refresh();
      break;

   default:;
   }
}

void Connection::onDisconnected()
{
   this->stopListening();
   this->deleteLater();
}
