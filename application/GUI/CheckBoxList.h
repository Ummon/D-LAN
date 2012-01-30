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

      bool eventFilter(QObject* object, QEvent* event);
      void paintEvent(QPaintEvent* );
      QSize sizeHint() const;

   protected:
      void mousePressEvent(QMouseEvent* e);
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
