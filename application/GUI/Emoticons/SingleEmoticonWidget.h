#ifndef GUI_SINGLEEMOTICONWIDGET_H
#define GUI_SINGLEEMOTICONWIDGET_H

#include <QWidget>
#include <QPixmap>
#include <QString>
#include <QStringList>

namespace Ui {
   class SingleEmoticonWidget;
}

namespace GUI
{
   class SingleEmoticonWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit SingleEmoticonWidget(QWidget* parent = 0);
      ~SingleEmoticonWidget();

      void setImage(const QPixmap& image);
      void setSymbols(const QStringList& list);

      void setTheme(const QString& theme);
      const QString& getTheme() const;

      void setEmoticonName(const QString& emoticonName);
      const QString& getEmoticonName() const;

   signals:
      void clicked();

   protected:
      void leaveEvent(QEvent* event) override;
      void enterEvent(QEvent* event) override;
      void	mousePressEvent(QMouseEvent* event) override;

   private:
      Ui::SingleEmoticonWidget* ui;

      QString themeName;
      QString emoticonName;
   };
}

#endif
