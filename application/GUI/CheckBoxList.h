#ifndef GUI_CHECKBOXLIST_H
#define GUI_CHECKBOXLIST_H

#include <QComboBox>
#include <QItemDelegate>
#include <QAbstractTableModel>

namespace GUI
{
   class CheckBoxModel : public QAbstractTableModel
   {
   public:
      CheckBoxModel();
      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      Qt::ItemFlags flags(const QModelIndex& index) const;

      void addElement(const QString& text, bool checked);
      bool isChecked(int row) const;
      void setChecked(int row, bool checked);
      void setText(int row, const QString& text);

   private:
      struct Item
      {
         Item(const QString& text, bool checked) : text(text), checked(checked) {}
         QString text;
         bool checked;
      };

      QList<Item> items;
   };

/////

   class CheckBoxList : public QComboBox
   {
      Q_OBJECT
   public:
      explicit CheckBoxList(QWidget* parent = 0);

      bool eventFilter(QObject *object, QEvent *event);
      void paintEvent(QPaintEvent *);
      QSize sizeHint() const;
      void addElement(const QString& text, bool checked);

   private:
      CheckBoxModel model;
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
