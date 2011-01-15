#ifndef RCC_ICORECONNECTION_H
#define RCC_ICORECONNECTION_H

#include <QObject>
#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/LogManager/IEntry.h>

namespace RCC
{
   class IBrowseResult;
   class ISearchResult;

   class ICoreConnection : public QObject
   {
      Q_OBJECT
   public:
      virtual ~ICoreConnection() {};

      virtual Common::Hash getOurID() const = 0;
      virtual void sendChatMessage(const QString& message) = 0;
      virtual void setCoreSettings(const Protos::GUI::CoreSettings settings) = 0;

      virtual QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID) = 0;
      virtual QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entry& entry) = 0;
      virtual QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots = true) = 0;

      virtual QSharedPointer<ISearchResult> search(const QString& terms) = 0;

      virtual void download(const Common::Hash& peerID, const Protos::Common::Entry& entry) = 0;
      virtual void cancelDownloads(const QList<quint64>& downloadIDs) = 0;
      virtual void moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, bool moveBefore = true) = 0;
      virtual void refresh() = 0;

      virtual bool isConnected() = 0;
      virtual bool isLocal() = 0;

   public slots:
      virtual void connectToCore() = 0;

   signals:
      void coreConnected();
      void coreDisconnected();

      void newState(const Protos::GUI::State&);
      void newChatMessage(const Common::Hash& peerID, const QString& message);
      void newLogMessage(QSharedPointer<const LM::IEntry> entry);
      void browseResult(const Protos::GUI::BrowseResult& browseResult);
      void searchResult(const Protos::Common::FindResult& findResult);
   };
}

#endif
