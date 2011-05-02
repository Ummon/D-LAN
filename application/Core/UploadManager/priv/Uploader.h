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
  
#ifndef UPLOADMANAGER_UPLOADER_H
#define UPLOADMANAGER_UPLOADER_H

#include <QThread>
#include <QSharedPointer>
#include <QTimer>
#include <QMutex>
#include <QWaitCondition>

#include <Common/Uncopyable.h>

namespace UM
{
   class Upload;

   class Uploader : public QThread, Common::Uncopyable
   {
      Q_OBJECT

   public:
      Uploader();
      ~Uploader();

      void startUploading(Upload* upload);
      Upload* getUpload() const;

      void startTimer();

   signals:
      void uploadFinished();
      void timeout();

   protected:
      void run();

   private:
      mutable QWaitCondition waitCondition;
      mutable QMutex mutex;

      bool toStop;
      bool active;

      Upload* upload;

      QTimer timer;
      QThread* mainThread;
   };
}
#endif
