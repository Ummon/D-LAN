#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>
#include <QLabel>
#include <QStyledItemDelegate>
#include <QPushButton>
#include <QIcon>
#include <QMdiSubWindow>
#include <QKeyEvent>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <PeerListModel.h>
#include <ChatModel.h>
#include <CoreConnection.h>

#include <WidgetSettings.h>
#include <WidgetChat.h>
#include <WidgetDownloads.h>
#include <WidgetUploads.h>
#include <WidgetBrowse.h>
#include <WidgetSearch.h>

namespace Ui {
   class MainWindow;
}

namespace GUI
{
   class MainWindow : public QMainWindow
   {
      Q_OBJECT
   public:
      explicit MainWindow(QWidget* parent = 0);
      ~MainWindow();

   private slots:
      void coreConnected();
      void coreDisconnected();

      void displayContextMenuPeers(const QPoint& point);
      void browse();
      void search();

      void removeWidget(QWidget* widget);

   protected:
      void keyPressEvent(QKeyEvent* event);

   private:
      void saveWindowsSettings();
      void restoreWindowsSettings();

      void removeMdiSubWindow(QMdiSubWindow* mdiSubWindow);

      void addWidgetSettings();

      void addWidgetChat();      
      void removeWidgetChat();

      void addWidgetDownloads();
      void removeWidgetDownloads();

      void addWidgetUploads();
      void removeWidgetUploads();

      void addWidgetBrowse(const Common::Hash& peerID);
      void addWidgetSearch(const QString& term);
      void removeAllWidgets();

      Ui::MainWindow *ui;

      WidgetSettings* widgetSettings;
      WidgetChat* widgetChat;
      WidgetDownloads* widgetDownloads;
      WidgetUploads* widgetUploads;
      QList<WidgetBrowse*> widgetsBrowse;
      QList<WidgetSearch*> widgetsSearch;

      CoreConnection coreConnection;

      PeerListModel peerListModel;
   };

   class PeerTableDelegate : public QStyledItemDelegate
   {
      Q_OBJECT
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   /**
     * @class TabCloseButton
     * Copied from 'qtabbar_p.h'.
     */
   class TabCloseButton : public QAbstractButton
   {
       Q_OBJECT
   public:
       TabCloseButton(QWidget* widget);

       QSize sizeHint() const;
       inline QSize minimumSizeHint() const { return sizeHint(); }
       void enterEvent(QEvent *event);
       void leaveEvent(QEvent *event);
       void paintEvent(QPaintEvent *event);

   signals:
      void clicked(QWidget* widget);

   private slots:
      void buttonClicked();

   private:
       QWidget* widget;
   };
}

#endif
