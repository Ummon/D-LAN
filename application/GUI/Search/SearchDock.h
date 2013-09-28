#ifndef GUI_SEARCHDOCK_H
#define GUI_SEARCHDOCK_H

#include <QDockWidget>
#include <QSharedPointer>

#include <Common/RemoteCoreController/ICoreConnection.h>

namespace Ui {
   class SearchDock;
}

namespace GUI
{
   class SearchDock : public QDockWidget
   {
      Q_OBJECT

   public:
      explicit SearchDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = 0);
      ~SearchDock();

      void setFocusToLineEdit();

   signals:
      void search(const QString& terms, bool ownFiles);

   protected:
      bool eventFilter(QObject* obj, QEvent* event);

   private slots:
      void coreConnected();
      void coreDisconnected(bool force);

      void searchOtherPeers();
      void searchOwnFiles();

   private:
      void search(bool ownFiles = false);

      Ui::SearchDock* ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
   };
}

#endif
