/**
  * Aybabtu - A decentralized LAN file sharing software.
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
  
#ifndef NETWORKLISTENER_ISEARCH_H
#define NETWORKLISTENER_ISEARCH_H

#include <QObject>
#include <QString>

#include <Protos/common.pb.h>

namespace NL
{
   class ISearch : public QObject
   {
      Q_OBJECT
   public:
      virtual ~ISearch() {}

      /**
        * Begin a new search. This function can be called only ONE time.
        * @return An associated tag. This tag will be repeated in the result, see the signal 'found'.
        */
      virtual quint64 search(const QString& words) = 0;

      /**
        * @return ms elapsed from the call to 'search'.
        */
      virtual qint64 elapsed() = 0;

   signals:
      void found(const Protos::Common::FindResult& result);
   };
}
#endif
