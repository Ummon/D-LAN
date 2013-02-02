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

   protected:
      bool event(QEvent* e);

   private:
      QList<KeyCombination> keyCombinationIgnored;
   };
}
#endif
