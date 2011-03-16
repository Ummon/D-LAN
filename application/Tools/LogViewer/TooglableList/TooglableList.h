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
  
#ifndef TOOGLABLELIST_H
#define TOOGLABLELIST_H

#include <QWidget>

#include <TooglableList/TooglableListButton.h>

namespace Ui { class TooglableList; }

class TooglableList : public QWidget
{
   Q_OBJECT
public:
   TooglableList(QWidget* parent = 0);
   ~TooglableList();

   void setList(const QStringList& list);
   QStringList getList();

signals:
   void stateChanged();

public slots:
   void addItem(const QString& item);
   void checkAll();
   void checkOne(QPushButton& but);

private slots:
   void butToogled(bool);
   void butRightClicked();

private:
   void clear();

   bool disableSignalStateChanged; ///< When all buttons are checked ('checkAll') it's usefull to avoid multiple signal sent.

   Ui::TooglableList* ui;
};

#endif
