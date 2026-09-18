#pragma once

#include <QTableView>
#include <QTimer>
#include <QSet>

#include <TableLogItemDelegate.h>

// Fixed-height rows normally; multiline rows are measured as they enter the viewport.
class LogTableView : public QTableView
{
public:
   explicit LogTableView(QWidget* parent = nullptr);
   void setModel(QAbstractItemModel* model) override;
   void setShowMultipleLines(bool enabled);

protected:
   void scrollContentsBy(int dx, int dy) override;
   void resizeEvent(QResizeEvent* event) override;
   void changeEvent(QEvent* event) override;

private:
   void invalidateLayout();
   void updateVisibleRows();

   TableLogItemDelegate delegate;
   QTimer layoutTimer;
   bool multipleLines = false;
   QSet<int> tallRows;
   QList<QMetaObject::Connection> modelConnections;
};
