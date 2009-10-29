#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QSharedPointer>

#include <INetworkListener.h>

#include <Common/LogManager/ILogger.h>
#include <FileManager/IFileManager.h>
#include <NetworkListener/INetworkListener.h>
#include <NetworkListener/IChat.h>
#include <PeerManager/IPeerManager.h>

using namespace NL;

namespace DM { class IDownloadManager; }
namespace UM { class IUploadManager; }

class Tests : public QObject
{
Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void testSending();
   void testReception();
   void messageRecevied(const Protos::Core::ChatMessage& message);


private :
   QSharedPointer<LM::ILogger> logger;
   QSharedPointer<FM::IFileManager> fileManager;
   QSharedPointer<NL::INetworkListener> networkListener;
   QSharedPointer<PM::IPeerManager> peerManager;
   bool isMessageRecevied;
};

#endif
