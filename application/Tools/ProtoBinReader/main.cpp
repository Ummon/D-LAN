#include <QtCore/QCoreApplication>

#include <iostream>

#include <QStringList>
#include <QTextStream>
#include <QFile>

#include <google/protobuf/message.h>
#include <google/protobuf/text_format.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>

#include <Protos/common.pb.h>
#include <Protos/files_cache.pb.h>
#include <Protos/queue.pb.h>


int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    QTextStream out(stdout);

    QStringList fileTypes;
    fileTypes << "cache";
    fileTypes << "queue";

    if (a.arguments().contains("-h") || a.arguments().contains("--help") || a.arguments().size() != 3 || !fileTypes.contains(a.arguments()[1].toLower()))
    {
       out << "Usage: " << a.arguments()[0] << " (cache | queue) <file>" << endl;
       return 1;
    }

    QFile file(a.arguments()[2]);
    if (!file.open(QIODevice::ReadOnly))
    {
       out << "Unable to read the file " << a.arguments()[2] << endl;
       return 1;
    }

    google::protobuf::Message* message;
    if (a.arguments()[1].toLower() == fileTypes[0])
       message = new Protos::FileCache::Hashes();
    else
       message = new Protos::Queue::Queue();

    if (!message->ParsePartialFromFileDescriptor(file.handle()))
    {
       out << "Parse error for the file " << a.arguments()[2] << endl;
       return 1;
    }

    google::protobuf::io::OstreamOutputStream outputStream(&std::cout);
    google::protobuf::TextFormat::Print(*message, &outputStream);
}
