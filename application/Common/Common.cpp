#include "Common.h"
using namespace Common;

#include <QDir>

#include <Constants.h>

bool Global::createApplicationFolder()
{
   if (!QDir::home().exists(APPLICATION_FOLDER_NAME))
      return QDir::home().mkdir(APPLICATION_FOLDER_NAME);

   return true;
}
