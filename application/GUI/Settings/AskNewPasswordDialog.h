#ifndef GUI_ASKNEWPASSWORDDIALOG_H
#define GUI_ASKNEWPASSWORDDIALOG_H

#include <QDialog>

#include <Common/Hash.h>

namespace Ui {
   class AskNewPasswordDialog;
}

namespace GUI
{
   class AskNewPasswordDialog : public QDialog
   {
      Q_OBJECT

   public:
      explicit AskNewPasswordDialog(const Common::Hash& oldPassword, QWidget* parent = 0);
      ~AskNewPasswordDialog();

      Common::Hash getNewPassword() const;

   private slots:
      void ok();

   private:
      Ui::AskNewPasswordDialog* ui;

      Common::Hash oldPassword;
   };
}

#endif
