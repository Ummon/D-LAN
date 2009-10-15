#include <priv/Search.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>
#include <priv/UDPListener.h>

/**
 * Create a new Search object
 *
 * @author mcuony
 */

::NetworkListener::Search::Search(UDPListener* newUdpListener, QSharedPointer<PeerManager::IPeerManager> newPeerManager)
{
   this->udpListener = newUdpListener;
   this->peerManager = newPeerManager;
   this->searchLaunched = false;
   this->tag = QTime::currentTime().second() * 1000 + (qrand() % 999);   //Sould be random enought, don't forget a search die after 15s.

   this->logger = LogManager::Builder::newLogger("NetworkListener::Search[" + QString::number(this->tag) + "]");

   this->logger->log("New search", LogManager::Debug);
}

/**
 * Begin a new search. This function can be called only ONE time
 *
 * @author mcuony
 */
bool ::NetworkListener::Search::search(const QString& words)
{
   if (this->searchLaunched)
   {
      this->logger->log("You can't launch a search twice !", LogManager::Error);
      return false;
   }
   else
   {

      // We put info in our find Proto.
      Protos::Core::Find findProto;
      findProto.set_pattern(words.toStdString());
      findProto.set_tag(this->tag);
      findProto.mutable_peerid()->set_hash(this->peerManager->getMyId().data());

      // We serialize the proto to a string.
      std::string output;
      findProto.SerializeToString(&output);


      this->logger->log("Search launched ! (" + words + ")", LogManager::Debug);

      this->searchLaunched = true;
      this->dateOfLaunch =  QDateTime::currentDateTime();

      //We listen for new search results
      Search::connect(this->udpListener, SIGNAL(newFindResult(const Protos::Common::FindResult&)), this, SLOT(newFindResult(const Protos::Common::FindResult&)));

      // We broadcast the data.
      return this->udpListener->sendMessage(findPacket + QString::fromStdString(output));
   }

}

/**
 * Called we a result is recevied : If the tag match, we forward to our listeners
 *
 * @author mcuony
 */
void ::NetworkListener::Search::newFindResult(const Protos::Common::FindResult& result) {

   this->logger->log("Search result" + QString::number(result.tag()), LogManager::Debug);

   if (result.tag() == this->tag)
   {
      this->logger->log("Find result for me !", LogManager::Debug);
      emit newFindResult(result);
   }
}
