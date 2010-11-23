// See examples of using in the components as FileManager.
struct Log { static QSharedPointer<LM::ILogger> logger; };
#define L_USER(mess) LOG_USER(Log::logger, mess)
#define L_DEBU(mess) LOG_DEBU(Log::logger, mess)
#define L_WARN(mess) LOG_WARN(Log::logger, mess)
#define L_ERRO(mess) LOG_ERRO(Log::logger, mess)
#define L_FATA(mess) LOG_FATA(Log::logger, mess)
