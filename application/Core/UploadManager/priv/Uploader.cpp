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
  
#include <priv/Uploader.h>
using namespace UM;

#include <QByteArray>
#include <QSharedPointer>

#include <Common/Settings.h>

#include <priv/Constants.h>
#include <priv/Log.h>
#include <priv/Upload.h>

/**
  * @class UM::Uploader
  *
  * An uploader is used to send one chunk to one peer via one socket.
  * The transfert is threaded.
  * The signal 'uploadFinished' is emitted when the upload is finished or if there is an error with the socket.
  */

Uploader::Uploader() :
   toStop(false), active(false), upload(0)
{
   this->mainThread = QThread::currentThread();

   this->timer.setInterval(SETTINGS.get<quint32>("upload_thread_lifetime"));
   this->timer.setSingleShot(true);
   connect(&this->timer, SIGNAL(timeout()), this, SIGNAL(timeout()));
   this->start();
}

Uploader::~Uploader()
{
   if (this->upload)
      this->upload->stop();

   this->mutex.lock();
   this->toStop = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();

   this->wait();

   L_DEBU(QString("Uploader deleted"));
}

void Uploader::startUploading(Upload* upload)
{
   this->mutex.lock();
   if (this->active)
   {
      L_DEBU("Uploader::start(..): Error, already active");
      this->mutex.unlock();
      return;
   }

   this->timer.stop();

   this->upload = upload;
   this->upload->moveSocketToThread(this);

   this->active = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
}

Upload* Uploader::getUpload() const
{
   return this->upload;
}

void Uploader::startTimer()
{
   this->timer.start();
   this->upload = 0;
}

void Uploader::run()
{
   forever
   {
      this->mutex.lock();
      if (!this->active && !this->toStop)
         this->waitCondition.wait(&this->mutex);
      if (this->toStop)
         return;
      this->mutex.unlock();

      this->upload->upload();

      this->upload->moveSocketToThread(this->mainThread);

      this->mutex.lock();
      this->active = false;
      this->mutex.unlock();

      emit uploadFinished();
   }
}
