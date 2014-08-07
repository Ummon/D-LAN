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
  
#ifndef GUI_AUTOCOMPLETE_H
#define GUI_AUTOCOMPLETE_H

#include <QWidget>
#include <QList>
#include <QPair>
#include <QAbstractItemModel>
#include <QSortFilterProxyModel>

#include <AutoComplete/AutoCompleteModel.h>
#include <Common/Hash.h>

namespace Ui {
   class AutoComplete;
}

namespace GUI
{
   class AutoComplete : public QWidget
   {
      Q_OBJECT
   public:
      explicit AutoComplete(QWidget* parent = 0);

      void setValues(const QList<QPair<Common::Hash, QString>>& values);

      Common::Hash getCurrent() const;

   signals:
      void stringAdded(QString str);
      void lastCharRemoved();

   protected:
      //void keyPressEvent(QKeyEvent* event) override;
      bool eventFilter(QObject* obj, QEvent* event) override;
      void showEvent(QShowEvent* event) override;

   private:
      Ui::AutoComplete* ui;

      QString currentPattern;

      AutoCompleteModel model;
      QSortFilterProxyModel filterModel;
   };
}

#endif
