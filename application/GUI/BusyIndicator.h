#ifndef GUI_BUSYINDICATOR_H
#define GUI_BUSYINDICATOR_H

#include <QWidget>
#include <QElapsedTimer>
#include <QTimer>
#include <QColor>
#include <QMetaType>

namespace GUI
{
   class BusyIndicator : public QWidget
   {
      Q_OBJECT
   public:
      BusyIndicator(QWidget* parent = 0, int refreshRate = 30, int animationDuration = 1200, int nbOfPoints = 12, int radiusOfThePoints = 20);

      QSize	sizeHint() const;

   protected:
      void paintEvent(QPaintEvent* event);
      void hideEvent(QHideEvent* event);
      void showEvent(QShowEvent* event);

   private:
      const int ANIMATION_DURATION; // Time to do a complete rotation.
      const int NB_OF_POINTS;
      const int RADIUS_OF_THE_POINTS;

      QTimer refreshTimer;
      QElapsedTimer timer;
   };
}

#endif
