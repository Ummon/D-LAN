#ifndef GUI_WIDGETSEARCH_H
#define GUI_WIDGETSEARCH_H

#include <QWidget>
#include <QString>
#include <QStyledItemDelegate>
#include <QPainter>

#include <CoreConnection.h>
#include <SearchModel.h>

namespace Ui {
   class WidgetSearch;
}

namespace GUI
{
   class SearchDelegate : public QStyledItemDelegate
   {
      static const QString MARKUP_FIRST_PART;
      static const QString MARKUP_SECOND_PART;

   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
      void setTerms(const QString& terms);

   private:
      QString toHtmlText(const QString& text) const;
      QStringList currentTerms;
   };

   class WidgetSearch : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetSearch(CoreConnection& coreConnection, PeerListModel& peerListModel, const QString& terms, QWidget *parent = 0);
      ~WidgetSearch();

   private slots:
      void displayContextMenuPeers(const QPoint& point);
      void download();
      void progress(int value);

   private:
      Ui::WidgetSearch *ui;
      CoreConnection& coreConnection;

      SearchModel searchModel;
      SearchDelegate searchDelegate;
   };
}

#endif
