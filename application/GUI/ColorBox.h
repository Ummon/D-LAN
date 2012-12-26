#ifndef COLORBOX_H
#define COLORBOX_H

#include <QPushButton>
#include <QColor>

namespace GUI
{
   class ColorBox : public QPushButton
   {
      Q_OBJECT
   public:
      explicit ColorBox(QWidget* parent = nullptr);

      void setColor(const QColor& color);

      QColor getCurrentColor() const;

   signals:
      void colorChanged(QColor newColor);

   public slots:
      void chooseColor();

   protected:
      virtual void paintEvent(QPaintEvent* event);

   private:
      QColor currentColor;

   };
}

#endif
