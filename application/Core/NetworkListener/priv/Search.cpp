#include <priv/Search.h>
using namespace NL;

#include <Common/ProtoHelper.h>

#include <priv/Log.h>

/**
  * @class Search
  * @author mcuony
  * @author gburri
  */

Search::Search(UDPListener& uDPListener)
   : uDPListener(uDPListener), tag(0)
{
   //this->searchLaunched = false;
}

/**
  * Begin a new search. This function can be called only ONE time
  */
void Search::search(const QString& words)
{
   if (this->tag != 0)
   {
      L_ERRO(QString("You can't launch a search twice! : %1").arg(words));
      return;
   }

   Protos::Core::Find findMessage;

   this->tag = this->mtrand.randInt();
   this->tag <<= 32;
   this->tag |= this->mtrand.randInt();
   findMessage.set_tag(this->tag);

   Common::ProtoHelper::setStr(findMessage, &Protos::Core::Find::set_pattern, words);

   connect(&this->uDPListener, SIGNAL(newFindResultMessage(Protos::Common::FindResult)), this, SLOT(newFindResult(Protos::Common::FindResult)));

   this->uDPListener.send(0x21, findMessage);
}

/**
  * Called when a result is recevied, if the tag matches, we forward the result to our listeners.
  */
void Search::newFindResult(const Protos::Common::FindResult& result)
{
   if (result.tag() == this->tag)
      emit found(result);
}
