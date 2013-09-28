#ifndef GUI_AUTOCOMPLETE_H
#define GUI_AUTOCOMPLETE_H

#include <QWidget>
#include <QList>
#include <QPair>
#include <QAbstractItemModel>

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

      void setValues(const QList<QPair<Common::Hash, QString>> values);
      void setFilter(const QString& pattern);

   signals:
      void selected(QPair<Common::Hash, QString> value);

   public slots:

   private:
      Ui::AutoComplete* ui;

      AutoCompleteModel model;
   };
}

#endif
