/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <CheckBoxList.h>
using namespace GUI;

#include <QCheckBox>
#include <QApplication>
#include <QStylePainter>
#include <QAbstractItemView>

#include <Log.h>

/**
  * @class GUI::CheckBoxList
  *
  * From : http://da-crystal.net/2008/06/checkbox-in-qcombobox/
  * The model given to CheckBoxList must have :
  * - Values of type bool for UserRole.
  * - First element must always exixst, it's used to check all the other items.
  */

CheckBoxList::CheckBoxList(QWidget *parent ) :
   QComboBox(parent)
{
   this->view()->setItemDelegate(new CheckBoxListDelegate(this));

   // Enable editing on items view.
   // TODO: There is a bug, the first element when the listview pop up is not editable!!! (Qt bug??)
   this->view()->setEditTriggers(QAbstractItemView::AllEditTriggers);

   // Set "CheckBoxList::eventFilter" as event filter for items view.
   this->view()->viewport()->installEventFilter(this);

   // It just cool to have it as default ;).
   this->view()->setAlternatingRowColors(true);
}

bool CheckBoxList::eventFilter(QObject* object, QEvent* event)
{
   // Don't close items view after we release the mouse button
   // by simple eating MouseButtonRelease in viewport of items view.
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

   if (this->model()->rowCount() > 0)
   {
      QString label;
      if (this->model()->data(this->model()->index(0, 0), Qt::UserRole).toBool())
         label = this->model()->data(this->model()->index(0, 0)).toString();
      else
      {
         bool nothingChecked = true;
         bool first = true;
         for (int i = 1; i < this->count(); i++)
         {
            if (this->model()->data(this->model()->index(i, 0), Qt::UserRole).toBool())
            {
               nothingChecked = false;
               if (first)
                  first = false;
               else
                  label += " + ";
               label += this->model()->data(this->model()->index(i, 0)).toString();
            }
         }
         if (nothingChecked)
            label = "<nothing>";
      }
      opt.currentText = label;
   }

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
      label += this->model()->data(this->model()->index(i, 0)).toString();
   }

   size.setWidth(fontMetrics.boundingRect(label).width());
   return size;
}

/**
  * A dirty trick to enable the edition of the first element.
  * @see CheckBoxList::CheckBoxList(..).
  */
void CheckBoxList::mousePressEvent(QMouseEvent* e)
{
   QComboBox::mousePressEvent(e);
   this->view()->edit(this->view()->model()->index(0, 0));
}

/////

CheckBoxListDelegate::CheckBoxListDelegate(QObject* parent) :
   QItemDelegate(parent)
{
}

void CheckBoxListDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   // Fill style options with item data.
   const QStyle* style = QApplication::style();

   QStyleOptionButton opt;
   opt.state |= index.model()->data(index, Qt::UserRole).toBool() ? QStyle::State_On : QStyle::State_Off;
   opt.state |= QStyle::State_Enabled;
   opt.text = index.model()->data(index, Qt::DisplayRole).toString();
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
   // Set editor data.
   QCheckBox* myEditor = static_cast<QCheckBox*>(editor);

   myEditor->setText(index.model()->data(index, Qt::DisplayRole).toString());
   myEditor->setChecked(index.model()->data(index, Qt::UserRole).toBool());
}

void CheckBoxListDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const
{
   // Get the editor.
   QCheckBox* myEditor = static_cast<QCheckBox*>(editor);

   // Set model data.
   model->setData(index, myEditor->text(), Qt::EditRole);
   model->setData(index, myEditor->isChecked(), Qt::UserRole);
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
