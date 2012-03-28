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
  
#ifndef COMMON_MESSAGE_HEADER_H
#define COMMON_MESSAGE_HEADER_H

#include <QIODevice>
#include <QDataStream>
#include <QString>

#include <Common/Hash.h>

namespace Common
{
   class notEnoughDataException {};

   class MessageHeader
   {
   public:
      enum MessageType
      {
         NULL_MESS =                   0x0000,

         /***** CORE *****/

         // UDP.
         CORE_IM_ALIVE =               0x0001,
         CORE_CHUNKS_OWNED =           0x0002,

         CORE_CHAT_MESSAGE =           0x0011,

         CORE_FIND =                   0x0021,
         CORE_FIND_RESULT =            0x0022,

         // TCP.
         CORE_GET_ENTRIES =            0x0031,
         CORE_GET_ENTRIES_RESULT =     0x0032,

         CORE_GET_HASHES =             0x0041,
         CORE_GET_HASHES_RESULT =      0x0042,
         CORE_HASH =                   0x0043,

         CORE_GET_CHUNK =              0x0051,
         CORE_GET_CHUNK_RESULT =       0x0052,

         /***** GUI *****/
         GUI_STATE =                   0x1001,
         GUI_STATE_RESULT =            0x1002,

         GUI_EVENT_CHAT_MESSAGES =     0x1011,
         GUI_EVENT_LOG_MESSAGE =       0x1012,

         GUI_ASK_FOR_AUTHENTICATION =  0x1021,
         GUI_AUTHENTICATION =          0x1022,
         GUI_AUTHENTICATION_RESULT =   0x1023,

         GUI_LANGUAGE =                0x10B1,

         GUI_CHANGE_PASSWORD =         0x10D1,

         GUI_SETTINGS =                0x1031,

         GUI_SEARCH =                  0x1041,
         GUI_SEARCH_TAG =              0x1042,
         GUI_SEARCH_RESULT =           0x1043,

         GUI_BROWSE =                  0x1051,
         GUI_BROWSE_TAG =              0x1052,
         GUI_BROWSE_RESULT =           0x1053,

         GUI_CANCEL_DOWNLOADS =        0x1061,
         GUI_PAUSE_DOWNLOADS =         0x10C1,
         GUI_MOVE_DOWNLOADS =          0x1071,

         GUI_DOWNLOAD =                0x1081,

         GUI_CHAT_MESSAGE =            0x1091,

         GUI_REFRESH =                 0x10A1
      };      

      MessageHeader();
      MessageHeader(MessageType type, quint32 size, const Hash senderID);

      const Hash& getSenderID() const;
      quint32 getSize() const;
      MessageType getType() const;

      bool isNull() const;
      void setNull();
      QString toStr() const;

      static QString messToStr(MessageType type);

      static MessageHeader readHeader(QIODevice& device, bool skipReadData = true);
      static MessageHeader readHeader(const char* data);
      static void writeHeader(QIODevice& device, const MessageHeader& header);
      static void writeHeader(char* buffer, const MessageHeader& header);

   private:
      static void writeHeader(QDataStream& stream, const MessageHeader& header);

      MessageType type;
      quint32 size;
      Hash senderID;

   public:
      static const int HEADER_SIZE;
   };
}

#endif
