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
  
#ifndef COMMON_DELETABLE_H
#define COMMON_DELETABLE_H

#include <exception>
using namespace std;

namespace Common
{
   class DeletedException : public exception {};

   class Deletable
   {
   public:
      Deletable() : deleted(false) {}
      virtual ~Deletable() {}

      virtual void setDeleted() { this->deleted = true; }
      virtual bool isDeleted() { return this->deleted; }

   protected:
      virtual void checkDeleted() { if (this->deleted) throw DeletedException(); }

   private:
      bool deleted;
   };
}

#endif
