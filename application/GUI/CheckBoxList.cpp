#include <CheckBoxList.h>
using namespace GUI;

#include <QCHeckBox>
#include <QApplication>
#include <QStylePainter>
#include <QAbstractItemView>

#include <Log.h>

/**
  * @class CheckBoxModel
  * Owns the items. Each item has a state checked or unchecked.
  * The first item is "<all>" and permits to check all other items.
  */

CheckBoxModel::CheckBoxModel()
{
   this->items << Item("<all>", true);
}

int CheckBoxModel::rowCount(const QModelIndex&) const
{
   return this->items.size();
}

int CheckBoxModel::columnCount(const QModelIndex&) const
{
   return 1;
}

void CheckBoxModel::addElement(const QString& text, bool checked)
{
   this->beginInsertRows(QModelIndex(), this->items.size(), this->items.size());
   this->items << Item(text, checked);
   this->endInsertRows();
}

QVariant CheckBoxModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->items.size())
      return QVariant();

   switch(role)
   {
   case Qt::DisplayRole:
      return this->items[index.row()].text;

   default:
      return QVariant();
   }
}

Qt::ItemFlags CheckBoxModel::flags(const QModelIndex& index) const
{
   Qt::ItemFlags defaultFlags = QAbstractItemModel::flags(index);

   if (index.isValid())
       return Qt::ItemIsEditable | defaultFlags;

   return defaultFlags;
}

bool CheckBoxModel::isChecked(int row) const
{
   if (row >= this->items.size())
      return false;
   return this->items[row].checked;
}

void CheckBoxModel::setChecked(int row, bool checked)
{
   if (row >= this->items.size() || this->items[row].checked == checked)
      return;

   this->items[row].checked = checked;

   if (row == 0)
   {
      for (int i = 1; i < this->items.size(); i++)
         this->items[i].checked = checked;
   }
   else
   {
      if (checked)
      {
         bool allChecked = true;
         for (int i = 1; i < this->items.size(); i++)
            if (!this->items[i].checked)
            {
               allChecked = false;
               break;
            }
         if (allChecked)
            this->items[0].checked = true;
      }
      else
         this->items[0].checked = false;
   }

   // There is too few elements to optimize, we say that all elements has changed.
   emit dataChanged(this->index(0, 0), this->index(this->items.size()-1, 0));
}

void CheckBoxModel::setText(int row, const QString& text)
{
   if (row >= this->items.size())
      return;
   this->items[row].text = text;
}

/////

/**
  * @class CheckBoxList
  * From : http://da-crystal.net/2008/06/checkbox-in-qcombobox/
  */

CheckBoxList::CheckBoxList(QWidget *parent )
   : QComboBox(parent)
{
   this->setModel(&this->model);

   //this->view()->setItemDelegate(new CheckBoxListDelegate(this));

   // Enable editing on items view.
   // TODO : There is a bug, the first element when the listview pop up is not editable!!! (Qt bug??)
   this->view()->setEditTriggers(QAbstractItemView::AllEditTriggers);

   // Set "CheckBoxList::eventFilter" as event filter for items view.
   this->view()->viewport()->installEventFilter(this);

   // It just cool to have it as default ;).
   this->view()->setAlternatingRowColors(true);
}

bool CheckBoxList::eventFilter(QObject* object, QEvent* event)
{
   // Don't close items view after we release the mouse button
   // by simple eating MouseButtonRelease in viewport of items view
   if(event->type() == QEvent::MouseButtonRelease && object == this->view()->viewport())
   {
      return true;
   }

   return QComboBox::eventFilter(object, event);
}

/**
  * Overloaded to print the checked item texts in the combobox.
  */
void CheckBoxList::paintEvent(QPaintEvent*)
{
   QStylePainter painter(this);
   painter.setPen(palette().color(QPalette::Text));

   // Draw the combobox frame, focusrect and selected etc.
   QStyleOptionComboBox opt;
   initStyleOption(&opt);

   QString label;
   if (this->model.isChecked(0))
      label = this->model.data(this->model.index(0, 0)).toString();
   else
   {
      bool nothingChecked = true;
      bool first = true;
      for (int i = 1; i < this->count(); i++)
      {
         if (this->model.isChecked(i))
         {
            nothingChecked = false;
            if (first)
               first = false;
            else
               label += " + ";
            label += this->model.data(this->model.index(i, 0)).toString();
         }
      }
      if (nothingChecked)
         label = "<nothing>";
   }
   opt.currentText = label;

   painter.drawComplexControl(QStyle::CC_ComboBox, opt);

   // Draw the icon and text.
   painter.drawControl(QStyle::CE_ComboBoxLabel, opt);
}

QSize CheckBoxList::sizeHint() const
{
   QSize size = QComboBox::sizeHint();
   QFontMetrics fontMetrics = this->fontMetrics();

   QString label;
   bool first = true;
   for (int i = 1; i < this->count(); i++)
   {
      if (first)
         first = false;
      else
         label += " + ";
      label += this->model.data(this->model.index(i, 0)).toString();
   }

   size.setWidth(fontMetrics.boundingRect(label).width());
   return size;
}

void CheckBoxList::addElement(const QString& text, bool checked)
{
   this->model.addElement(text, checked);
}

/////

CheckBoxListDelegate::CheckBoxListDelegate(QObject* parent)
   : QItemDelegate(parent)
{
}

void CheckBoxListDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   // Get the model data.
   const CheckBoxModel* model = static_cast<const CheckBoxModel*>(index.model());

   // Fill style options with item data.
   const QStyle* style = QApplication::style();

   QStyleOptionButton opt;
   opt.state |= model->isChecked(index.row()) ? QStyle::State_On : QStyle::State_Off;
   opt.state |= QStyle::State_Enabled;
   opt.text = model->data(index).toString();
   opt.rect = option.rect;

   // Draw item data as CheckBox.
   style->drawControl(QStyle::CE_CheckBox, &opt, painter);
}

QWidget* CheckBoxListDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   // Create check box as our editor.
   QCheckBox* editor = new QCheckBox(parent);
   connect(editor, SIGNAL(stateChanged(int)), this, SLOT(checkBoxStateChanged()));
   return editor;
}

void CheckBoxListDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const
{
   // If we want to be model-independant we can use the role 'UserRole' to save the checked state into.
   // This is not the case here, we know the concrete type of the model.
   const CheckBoxModel* myModel = static_cast<const CheckBoxModel*>(index.model());

   // Set editor data.
   QCheckBox* myEditor = static_cast<QCheckBox*>(editor);
   myEditor->setText(myModel->data(index).toString());
   myEditor->setChecked(myModel->isChecked(index.row()));
}

void CheckBoxListDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const
{
   // Get the model data.
   CheckBoxModel* myModel = static_cast<CheckBoxModel*>(model);

   // Get the editor.
   QCheckBox* myEditor = static_cast<QCheckBox*>(editor);

   // Set model data.
   myModel->setChecked(index.row(), myEditor->isChecked());
   myModel->setText(index.row(), myEditor->text());
}

void CheckBoxListDelegate::updateEditorGeometry(QWidget* editor, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   editor->setGeometry(option.rect);
}

void CheckBoxListDelegate::checkBoxStateChanged()
{
   // The view commit data automatically when the widget lost the focus, we force to commit immediately.
   emit commitData(static_cast<QWidget*>(this->sender()));
}
