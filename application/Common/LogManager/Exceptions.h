#ifndef LOGMANAGER_EXCEPTIONS_H
#define LOGMANAGER_EXCEPTIONS_H

#include <exception>
using namespace std;

#include <QString>

namespace LM
{
   class LoggerAlreadyExistsException : public exception {};

   class MalformedEntryLog : public exception
   {
   public:
      MalformedEntryLog(const QString& message) :
         message(message)
      {}
      virtual ~MalformedEntryLog() throw() {}

      const QString message;
   };
}
#endif
