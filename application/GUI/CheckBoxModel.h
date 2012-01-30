/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef GUI_CHECKBOXMODEL_H
#define GUI_CHECKBOXMODEL_H

#include <QAbstractTableModel>

#include <IFilter.h>

namespace GUI
{
   template <typename T>
      class CheckBoxModel : public QAbstractTableModel, public IFilter<T>
   {
   public:
      CheckBoxModel();
      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;
      bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole);
      Qt::ItemFlags flags(const QModelIndex& index) const;

      QList<T> getFilteredValues() const;

      void addElement(const QString& text, bool checked, T value);

   private:
      void setChecked(int row, bool checked);
      void setText(int row, const QString& text);

      struct Item
      {
         Item(const QString& text, bool checked) : text(text), checked(checked) {}
         Item(const QString& text, bool checked, T value) : text(text), checked(checked), value(value) {}

         QString text;
         bool checked;
         T value;
      };

      QList<Item> items;
   };
}

/***** Definition *****/
using namespace GUI;

/**
  * @class GUI::CheckBoxModel
  *
  * Owns the items. Each item has a state checked or unchecked.
  * The first item is "<All>" and permits to check all other items.
  */

template <typename T>
CheckBoxModel<T>::CheckBoxModel()
{
   this->items << Item("<All>", true);
}

template <typename T>
int CheckBoxModel<T>::rowCount(const QModelIndex&) const
{
   return this->items.size();
}

template <typename T>
int CheckBoxModel<T>::columnCount(const QModelIndex&) const
{
   return 1;
}

template <typename T>
QVariant CheckBoxModel<T>::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->items.size())
      return QVariant();

   switch(role)
   {
   case Qt::DisplayRole:
      return this->items[index.row()].text;
   case Qt::UserRole:
      return this->items[index.row()].checked;
   default:
      return QVariant();
   }
}

template <typename T>
bool CheckBoxModel<T>::setData(const QModelIndex& index, const QVariant& value, int role)
{
   if (!index.isValid() || index.row() >= this->items.size())
      return false;

   switch(role)
   {
   case Qt::EditRole:
      this->setText(index.row(), value.toString());
      return true;
   case Qt::UserRole:
      this->setChecked(index.row(), value.toBool());
      return true;
   default:
      return false;
   }
}

template <typename T>
Qt::ItemFlags CheckBoxModel<T>::flags(const QModelIndex& index) const
{
   Qt::ItemFlags defaultFlags = QAbstractItemModel::flags(index);

   if (index.isValid())
       return Qt::ItemIsEditable | defaultFlags;

   return defaultFlags;
}

template <typename T>
QList<T> CheckBoxModel<T>::getFilteredValues() const
{
   QList<T> values;
   QListIterator<Item> i(this->items);
   i.next(); // We don't care about "<All>".
   while (i.hasNext())
   {
      const Item& item = i.next();
      if (!item.checked)
         values << item.value;
   }
   return values;
}

template <typename T>
void CheckBoxModel<T>::addElement(const QString& text, bool checked, T value)
{
   this->beginInsertRows(QModelIndex(), this->items.size(), this->items.size());
   this->items << Item(text, checked, value);
   this->endInsertRows();
}

template <typename T>
void CheckBoxModel<T>::setChecked(int row, bool checked)
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

template <typename T>
void CheckBoxModel<T>::setText(int row, const QString& text)
{
   if (row >= this->items.size())
      return;
   this->items[row].text = text;
}

#endif
