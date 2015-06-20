/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
  
#include <limits>

#include <Common/ConsoleReader.h>
using namespace Common;

/**
  * @class ConsoleReader
  */

ConsoleReader::ConsoleReader(QObject* parent) :
   QObject(parent)
{
   Reader* reader = new Reader;
   reader->moveToThread(&this->readerThread);
   connect(&this->readerThread, QThread::finished, reader, QObject::deleteLater);
   connect(this, ConsoleReader::readNextLine, reader, Reader::readLine);
   connect(reader, Reader::lineRead, this, nextLine);
   this->readerThread.start();

   emit readNextLine();
}

void ConsoleReader::nextLine(QString line)
{
   QString lineTrimmed = line.trimmed();
   if (!lineTrimmed.isEmpty())
      emit newLine(lineTrimmed);
   emit readNextLine();
}

Reader::Reader()
   : inputStream(stdin)
{
}

void Reader::readLine()
{
   emit lineRead(this->inputStream.readLine());
}
