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

/**
  * @class EmoticonTextDocument
  * This class mainly exist because the QTextDocument::setMarkup clean all resources, I think this is a bug.
  */

#include <Chat/EmoticonTextDocument.h>
using namespace GUI;

EmoticonTextDocument::EmoticonTextDocument(const Emoticons& emoticons, QObject* parent) :
   QTextDocument(parent),
   emoticons(emoticons)
{
}

QVariant EmoticonTextDocument::loadResource(int type, const QUrl& name)
{
   if (type == QTextDocument::ImageResource && name.scheme() == "emoticons")
      return this->emoticons.getSmileImage(name.host(), name.path().mid(1)); // Skip the '/' at the beginning.
   return QTextDocument::loadResource(type, name);
}
