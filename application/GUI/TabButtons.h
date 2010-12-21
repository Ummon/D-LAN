#ifndef GUI_TABBUTTONS_H
#define GUI_TABBUTTONS_H

#include <QAbstractButton>
#include <QStyleOption>
#include <QPainter>

namespace GUI
{
   /**
     * @class TabButton
     * Inspired from 'qtabbar_p.h'.
     */
   class TabButton : public QAbstractButton
   {
      Q_OBJECT
   public:
      TabButton(QWidget* parent = 0);
      virtual QSize sizeHint() const;
      virtual QSize minimumSizeHint() const;
      virtual void enterEvent(QEvent *event);
      virtual void leaveEvent(QEvent *event);
      virtual void paintEvent(QPaintEvent *event);

   protected:
      virtual void drawPrimitive(const QStyleOption& opt, QPainter& p) = 0;
   };

/////

   class TabCloseButton : public TabButton
   {
       Q_OBJECT
   public:
       TabCloseButton(QWidget* widget, QWidget* parent = 0);

   protected:
       void drawPrimitive(const QStyleOption& opt, QPainter& p);

   signals:
      void clicked(QWidget* widget);

   private slots:
      void buttonClicked();

   private:
       QWidget* widget;
   };

/////

   class TabRefreshButton : public TabButton
   {
       Q_OBJECT
   public:
       TabRefreshButton(QWidget* parent = 0);

   private:
       void drawPrimitive(const QStyleOption& option, QPainter& painter);

       QIcon icon;
   };
}

#endif
