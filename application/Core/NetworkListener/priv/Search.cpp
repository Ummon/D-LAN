#include <priv/Search.h>
using namespace NL;

#include <Common/LogManager/Builder.h>
#include <priv/UDPListener.h>

/**
  * @class Search
  * @author mcuony
  * @author gburri
  */

Search::Search(UDPListener& uDPListener)
   : uDPListener(uDPListener)
{
   this->searchLaunched = false;
   this->tag = QTime::currentTime().second() * 1000 + (qrand() % 999);   //Sould be random enought, don't forget a search die after 15s.
}

/**
 * Begin a new search. This function can be called only ONE time
 *
 * @author mcuony
 */
bool Search::search(const QString& words)
{
//   if (this->searchLaunched)
//   {
//      LOG_ERRO(this->logger, "You can't launch a search twice !");
//      return false;
//   }
//   else
//   {
//      // We put info in our find Proto.
//      Protos::Core::Find findProto;
//      findProto.set_pattern(words.toStdString());
//      findProto.set_tag(this->tag);
//      findProto.mutable_peer_id()->set_hash(this->peerManager->getMyId().getData(), Common::Hash::HASH_SIZE);

//      // We serialize the proto to a string.
//      std::string output;
//      findProto.SerializeToString(&output);


//      LOG_DEBU(this->logger, "Search launched ! (" + words + ")");

//      this->searchLaunched = true;
//      this->dateOfLaunch =  QDateTime::currentDateTime();

//      //We listen for new search results
//      Search::connect(this->udpListener, SIGNAL(newFindResult(const Protos::Common::FindResult&)), this, SLOT(newFindResult(const Protos::Common::FindResult&)));

//      // We broadcast the data.
//      return this->udpListener->sendMessage(QByteArray(output.data()).prepend(findPacket));
//   }
   return true;
}

/**
 * Called we a result is recevied : If the tag match, we forward to our listeners
 *
 */
void Search::newFindResult(const Protos::Common::FindResult& result) {

   //this->logger->log("Search result" + QString::number(result.tag()), LogManager::Debug);

   if (result.tag() == this->tag)
   {
      //emit newFindResult(result);
   }
}
