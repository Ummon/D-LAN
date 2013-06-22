#ifndef GUI_EMOTICONSWIDGET_H
#define GUI_EMOTICONSWIDGET_H

#include <QWidget>
#include <QRadioButton>

#include <Emoticons/Emoticons.h>

namespace GUI
{
   class EmoticonsWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit EmoticonsWidget(Emoticons& emoticons, QWidget* parent = 0);

      void setDefaultTheme(const QString& theme);

   signals:
      void emoticonChoosen(const QString& theme, const QString& emoticonName);
      void defaultThemeChanged(const QString& theme);

   private slots:
      void emoticonClicked();
      void themeButtonToggled(bool checked);

   private:
      Emoticons& emoticons;
      QList<QRadioButton*> themeButtons;
   };
}

#endif
