#ifndef GUI_TEXTEDIT_H
#define GUI_TEXTEDIT_H

#include <QEvent>
#include <QTextEdit>
#include <QList>

namespace GUI
{
   struct KeyCombination
   {
      Qt::KeyboardModifier modifier;
      int key;
   };

   class ChatTextEdit : public QTextEdit
   {
      Q_OBJECT
   public:
      explicit ChatTextEdit(QWidget* parent = 0);

      void addIgnoreKeyCombination(KeyCombination keyCombination);

   signals:
      void wordTyped(int position, const QString&);

   protected:
      bool event(QEvent* e);

   private slots:
      void documentContentsChange(int position, int charsRemoved, int charsAdded);

   private:
      QList<KeyCombination> keyCombinationIgnored;
   };
}
#endif
