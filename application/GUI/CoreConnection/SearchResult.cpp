#include <CoreConnection/SearchResult.h>
using namespace GUI;

#include <Protos/gui_protocol.pb.h>

#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

#include <CoreConnection/CoreConnection.h>

SearchResult::SearchResult(CoreConnection* coreConnection, const QString& terms)
   : ISearchResult(SETTINGS.get<quint32>("socket_timeout")), coreConnection(coreConnection), terms(terms)
{
   connect(this->coreConnection, SIGNAL(searchResult(const Protos::Common::FindResult&)), this, SLOT(searchResult(const Protos::Common::FindResult&)));
}

void SearchResult::start()
{
   Protos::GUI::Search search;
   Common::ProtoHelper::setStr(search, &Protos::GUI::Search::set_pattern, this->terms);
   this->coreConnection->send(Common::Network::GUI_SEARCH, search);
}

void SearchResult::setTag(quint64 tag)
{
   this->tag = tag;
}

void SearchResult::searchResult(const Protos::Common::FindResult& findResult)
{
   if (findResult.tag() == this->tag) // Is this message for us?
      emit result(findResult);
}
