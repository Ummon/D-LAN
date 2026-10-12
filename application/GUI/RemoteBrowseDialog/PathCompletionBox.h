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

#pragma once

#include <QLineEdit>
#include <QListView>
#include <QStringListModel>

namespace GUI
{
   class PathCompletionBox : public QListView
   {
      Q_OBJECT
   public:
      PathCompletionBox(QLineEdit* lineEdit, QWidget* parent);

      void setCompletions(const QString& path, const QStringList& completions);
      bool isBrowsing() const;

   signals:
      // The text box holds a proposal the user goes through or has chosen, or again the typed path.
      void pathEdited(const QString& path);
      // The paths completing the one of the text box are to be given to 'setCompletions(..)'.
      void completionsWanted(const QString& path);

   protected:
      bool eventFilter(QObject* watched, QEvent* event) override;

   private:
      bool cursorPutAtEnd(QEvent* event) const;
      bool keyPressed(QKeyEvent* event);
      void browse(int row);
      void take(int row, bool completed);
      void dismiss(bool restoreTypedPath);
      void place();

      QLineEdit* lineEdit;
      QStringListModel paths;
      // What the text box held when the proposals were given.
      QString typedPath;
   };
}
