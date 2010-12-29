#ifndef GUI_REMOTEFILEDIALOG_H
#define GUI_REMOTEFILEDIALOG_H

#include <QDialog>

namespace Ui {
   class RemoteFileDialog;
}

namespace GUI
{
   class RemoteFileDialog : public QDialog
   {
      Q_OBJECT
   public:
      explicit RemoteFileDialog(QWidget *parent = 0);
      ~RemoteFileDialog();

      void setText(const QString& text);
      QString getFolder() const;

   private:
      Ui::RemoteFileDialog *ui;
   };
}

#endif
