#ifndef GUI_WIDGETSEARCH_H
#define GUI_WIDGETSEARCH_H

#include <QWidget>
#include <QString>

#include <CoreConnection.h>
#include <SearchModel.h>

namespace Ui {
   class WidgetSearch;
}

namespace GUI
{
   class WidgetSearch : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetSearch(CoreConnection& coreConnection, PeerListModel& peerListModel, const QString& terms, QWidget *parent = 0);
      ~WidgetSearch();

   private:
      Ui::WidgetSearch *ui;
      CoreConnection& coreConnection;

      SearchModel searchModel;
   };
}

#endif
