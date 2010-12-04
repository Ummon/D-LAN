#ifndef GUI_CHECKBOXLIST_H
#define GUI_CHECKBOXLIST_H

#include <QComboBox>
#include <QItemDelegate>
#include <QAbstractTableModel>

namespace GUI
{
   class CheckBoxList : public QComboBox
   {
      Q_OBJECT
   public:
      explicit CheckBoxList(QWidget* parent = 0);

      bool eventFilter(QObject *object, QEvent *event);
      void paintEvent(QPaintEvent *);
      QSize sizeHint() const;
   };

/////

   class CheckBoxListDelegate : public QItemDelegate
   {
      Q_OBJECT
   public:
      CheckBoxListDelegate(QObject* parent);

      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option ,const QModelIndex& index) const;
      void setEditorData(QWidget* editor, const QModelIndex& index) const;
      void setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const;
      void updateEditorGeometry(QWidget* editor, const QStyleOptionViewItem& option, const QModelIndex& index) const;

   private slots:
      void checkBoxStateChanged();
    };
}

#endif
