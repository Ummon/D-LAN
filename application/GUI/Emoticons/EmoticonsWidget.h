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

   signals:
      void hidden();
      void emoticonChoosen(const QString& theme, const QString& emoticonName);
      void defaultThemeChanged(const QString& theme);

   protected:
      void showEvent(QShowEvent* event) override;
      void hideEvent(QHideEvent* event) override;

   private slots:
      void setDefaultTheme(const QString& theme);
      void emoticonClicked();
      void themeButtonToggled(bool checked);

   private:
      Emoticons& emoticons;
      QList<QRadioButton*> themeButtons;
   };
}

#endif
