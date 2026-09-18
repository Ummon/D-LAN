#include <LogTableView.h>

#include <QEvent>
#include <QHeaderView>
#include <QScrollBar>

#include <TableLogModel.h>

LogTableView::LogTableView(QWidget* parent) : QTableView(parent)
{
   this->setItemDelegate(&this->delegate);
   this->setWordWrap(false);
   this->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
   this->verticalHeader()->setMinimumSectionSize(1);
   this->verticalHeader()->setDefaultSectionSize(this->fontMetrics().height() + 8);
   this->layoutTimer.setSingleShot(true);
   connect(&this->layoutTimer, &QTimer::timeout, this, &LogTableView::updateVisibleRows);
}

void LogTableView::setModel(QAbstractItemModel* model)
{
   for (const auto& connection : std::as_const(this->modelConnections))
      disconnect(connection);
   this->modelConnections.clear();
   this->invalidateLayout();
   QTableView::setModel(model);
   if (model)
   {
      this->modelConnections << connect(model, &QAbstractItemModel::modelAboutToBeReset, this, &LogTableView::invalidateLayout);
      this->modelConnections << connect(model, &QAbstractItemModel::modelReset, this, [this] { this->layoutTimer.start(); });
      this->modelConnections << connect(model, &QAbstractItemModel::dataChanged, this, &LogTableView::invalidateLayout);
      this->modelConnections << connect(model, &QAbstractItemModel::rowsInserted, this, [this] { this->layoutTimer.start(); });
      this->modelConnections << connect(model, &QAbstractItemModel::rowsAboutToBeRemoved, this, &LogTableView::invalidateLayout);
   }
}

void LogTableView::setShowMultipleLines(bool enabled)
{
   this->multipleLines = enabled;
   this->invalidateLayout();
}

void LogTableView::invalidateLayout()
{
   this->delegate.resetSizesCache();
   const int height = this->fontMetrics().height() + 8;
   for (int row : std::as_const(this->tallRows))
      if (row < this->verticalHeader()->count())
         this->setRowHeight(row, height);
   this->tallRows.clear();
   this->verticalHeader()->setDefaultSectionSize(height);
   this->layoutTimer.start();
   this->viewport()->update();
}

void LogTableView::updateVisibleRows()
{
   if (!this->multipleLines || !this->model() || this->model()->rowCount() == 0)
      return;

   const bool atBottom = this->verticalScrollBar()->value() == this->verticalScrollBar()->maximum();
   int row = this->rowAt(0);
   if (row < 0)
      return;
   QStyleOptionViewItem option;
   this->initViewItemOption(&option);
   const int defaultHeight = this->verticalHeader()->defaultSectionSize();
   bool resized = false;
   // Row positions change as heights are assigned; stop at the current viewport edge.
   for (; row < this->model()->rowCount() && this->rowViewportPosition(row) < this->viewport()->height(); ++row)
   {
      const auto index = this->model()->index(row, TableLogModel::MESSAGE);
      const int height = qMax(defaultHeight, this->delegate.sizeHint(option, index).height());
      if (height != this->rowHeight(row))
      {
         this->setRowHeight(row, height);
         resized = true;
      }
      if (height != defaultHeight)
         this->tallRows.insert(row);
   }
   if (atBottom && resized)
   {
      // Header geometry updates are deferred. Apply them before following the tail,
      // otherwise newly enlarged rows can push the last entries out of view.
      this->updateGeometries();
      this->scrollToBottom();
      this->layoutTimer.start();
   }
}

void LogTableView::scrollContentsBy(int dx, int dy)
{
   QTableView::scrollContentsBy(dx, dy);
   if (this->multipleLines)
      this->layoutTimer.start();
}

void LogTableView::resizeEvent(QResizeEvent* event)
{
   QTableView::resizeEvent(event);
   if (this->multipleLines)
      this->layoutTimer.start();
}

void LogTableView::changeEvent(QEvent* event)
{
   QTableView::changeEvent(event);
   if (event->type() == QEvent::FontChange || event->type() == QEvent::StyleChange)
      this->invalidateLayout();
}
