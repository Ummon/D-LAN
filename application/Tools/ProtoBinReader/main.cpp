/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
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
