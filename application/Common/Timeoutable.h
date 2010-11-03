#ifndef COMMON_TIMEOUTABLE_H
#define COMMON_TIMEOUTABLE_H

#include <QObject>
#include <QTimer>

namespace Common
{
   // An inteface 'ITimeoutable' should be great but the QObject system doesn't support diamond inheritance.
   class Timeoutable : public QObject
   {
      Q_OBJECT
   protected:
      Timeoutable(int time);

   public:
      virtual ~Timeoutable() {}
      bool isTimeouted() const;

   signals:
      void timeout();

   protected:
      void startTimer();
      void stopTimer();

   private slots:
      void timeoutSlot();

   private:
      bool timeouted;
      QTimer timer;
   };
}

#endif
