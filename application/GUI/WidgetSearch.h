#ifndef GUI_WIDGETSEARCH_H
#define GUI_WIDGETSEARCH_H

#include <QWidget>
#include <QString>

#include <CoreConnection.h>

namespace Ui {
   class WidgetSearch;
}

namespace GUI
{
   class WidgetSearch : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetSearch(CoreConnection& coreConnection, const QString& term, QWidget *parent = 0);
      ~WidgetSearch();

   private:
      Ui::WidgetSearch *ui;
      CoreConnection& coreConnection;
   };
}

#endif
