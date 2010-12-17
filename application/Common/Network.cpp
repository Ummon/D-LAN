#include <Common/Network.h>
using namespace Common;

QString Network::messToStr(CoreMessageType type)
{
   switch (type)
   {
   case CORE_IM_ALIVE: return "IM_ALIVE";
   case CORE_CHUNKS_OWNED: return "CHUNKS_OWNED";
   case CORE_CHAT_MESSAGE: return "CHAT_MESSAGE";
   case CORE_FIND: return "FIND";
   case CORE_FIND_RESULT: return "FIND_RESULT";
   case CORE_GET_ENTRIES: return "GET_ENTRIES";
   case CORE_GET_ENTRIES_RESULT: return "GET_ENTRIES_RESULT";
   case CORE_GET_HASHES: return "GET_HASHES";
   case CORE_GET_HASHES_RESULT: return "GET_HASHES_RESULT";
   case CORE_HASH: return "HASH";
   case CORE_GET_CHUNK: return "GET_CHUNK";
   case CORE_GET_CHUNK_RESULT: return "GET_CHUNK_RESULT";
   default: return "<UNKNOWN_CORE_MESSAGE_TYPE>";
   }
}

QString Network::messToStr(GUIMessageType type)
{
   switch (type)
   {
   case GUI_STATE: return "STATE";
   case GUI_EVENT_CHAT_MESSAGE:  return "EVENT_CHAT_MESSAGE";
   case GUI_EVENT_LOG_MESSAGE: return "EVENT_LOG_MESSAGE";
   case GUI_AUTHENTICATION: return "AUTHENTICATION";
   case GUI_AUTHENTICATION_RESULT: return "AUTHENTICATION_RESULT";
   case GUI_SETTINGS: return "SETTINGS";
   case GUI_SEARCH: return "SEARCH";
   case GUI_SEARCH_TAG: return "SEARCH_TAG";
   case GUI_SEARCH_RESULT: return "SEARCH_RESULT";
   case GUI_BROWSE: return "BROWSE";
   case GUI_BROWSE_TAG: return "BROWSE_TAG";
   case GUI_BROWSE_RESULT: return "BROWSE_RESULT";
   case GUI_CANCEL_DOWNLOADS: return "CANCEL_DOWNLOADS";
   case GUI_DOWNLOAD: return "DOWNLOAD";
   case GUI_CHAT_MESSAGE: return "CHAT_MESSAGE";
   case GUI_REFRESH: return "REFRESH";
   default: return "<UNKNOWN_GUI_MESSAGE_TYPE>";
   }
}
