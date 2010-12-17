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
  
#ifndef NETWORKLISTENER_SEARCH_H
#define NETWORKLISTENER_SEARCH_H

#include <QSharedPointer>
#include <QElapsedTimer>

#include <Libs/MersenneTwister.h>

#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>

#include <ISearch.h>
#include <priv/UDPListener.h>

namespace NL
{
   class Search : public ISearch, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Search(UDPListener& uDPListener);
      quint64 search(const QString& words);
      qint64 elapsed();

   private slots:
      void newFindResult(const Protos::Common::FindResult& result);

   private:
      UDPListener& uDPListener;

      bool searchLaunched;
      QDateTime dateOfLaunch;

      quint64 tag;
      MTRand mtrand;

      QElapsedTimer timer;
   };
}
#endif
