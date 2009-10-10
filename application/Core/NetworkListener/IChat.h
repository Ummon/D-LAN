#ifndef NETWORKMANAGER_ICHAT_H
#define NETWORKMANAGER_ICHAT_H

#include <QObject>
#include <QString>

#include <Protos/core_protocol.pb.h>

namespace NetworkListener
{
    class IChat : public QObject
    {
        Q_OBJECT
    public:
        virtual ~IChat() {}

        virtual void send(const QString& message) = 0;

    signals:
        void newMessage(const Protos::Core::ChatMessage& message);

        /* Dues to a limiation of QObject (cannot inherit more than one QObject class), we must have the def of
      newChatMessage, used in Chat, here. TODO: Find a better solution ? */
    public slots:
        virtual void newChatMessage(const Protos::Core::ChatMessage& message) = 0;
    };
}
#endif
