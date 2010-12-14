#include <MainWindow.h>
using namespace PasswordHasher;

#include <QMessageBox>

#include <Protos/common.pb.h>
#include <Protos/core_settings.pb.h>
#include <google/protobuf/text_format.h>

#include <Common/Hash.h>
#include <Common/Constants.h>
#include <Common/Settings.h>
#include <Common/PersistentData.h>

#include <ui_MainWindow.h>

MainWindow::MainWindow(QWidget *parent)
   : QMainWindow(parent), CORE_SETTINGS_PATH(Common::APPLICATION_FOLDER_PATH + '/' + Common::CORE_SETTINGS_FILENAME), ui(new Ui::MainWindow)
{
   ui->setupUi(this);
   this->setButtonText();
   this->computeHash();

   connect(this->ui->txtPass1, SIGNAL(textChanged(const QString&)), this, SLOT(computeHash()));
   connect(this->ui->txtPass2, SIGNAL(textChanged(const QString&)), this, SLOT(computeHash()));

   connect(this->ui->butSave, SIGNAL(clicked()), this, SLOT(savePassword()));

   this->ui->lblInstructions->setText(this->ui->lblInstructions->text().replace("{settings_path}", CORE_SETTINGS_PATH));

   SETTINGS.setFilename(Common::CORE_SETTINGS_FILENAME);
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());
}

MainWindow::~MainWindow()
{
   delete ui;
}

void MainWindow::computeHash()
{
   QString error = this->checkPasswords();
   if (!error.isNull())
   {
      this->ui->txtResult->setText(error);
   }
   else
   {
      Common::Hash hash = Common::Hasher::hashWithSalt(this->ui->txtPass1->text());

      Protos::Common::Hash hashMessage;
      const google::protobuf::FieldDescriptor* hashField = hashMessage.GetDescriptor()->FindFieldByName("hash");

      std::string encodedHash;
      hashMessage.set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      google::protobuf::TextFormat::PrintFieldValueToString(hashMessage, hashField, -1, &encodedHash);

      this->ui->txtResult->setText("remote_password {\n   hash: " + QString::fromStdString(encodedHash) + "\n}");
   }
}

void MainWindow::savePassword()
{
   QString error = this->checkPasswords();

   if (!error.isNull())
   {
      QMessageBox::warning(this, "Password cannot be saved", error);
   }
   else
   {
      try
      {
         SETTINGS.load();
         SETTINGS.set("remote_password", Common::Hasher::hashWithSalt(this->ui->txtPass1->text()));
         SETTINGS.save();
         QMessageBox::information(this, "Password saved", "Password has been saved.");
      }
      catch (Common::PersistentDataIOException& e)
      {
         QMessageBox::warning(this, "Error during saving", e.message);
      }
   }
}

void MainWindow::setButtonText()
{
   this->ui->butSave->setText(QString("Save result to \"%1\"").arg(CORE_SETTINGS_PATH));
}

/**
  * Checks that password are not empty, are equal and do not contain any whitespace.
  * @return An error message if there is an error or a null string if everything is fine.
  */
QString MainWindow::checkPasswords() const
{
   if (this->ui->txtPass1->text() != this->ui->txtPass2->text())
      return QString("Error: passwords aren't the same");
   else if (this->ui->txtPass1->text().isEmpty())
      return QString("Error: password is empty");
   else if (this->ui->txtPass1->text().contains(QRegExp("\\s")))
      return QString("Error: password contains one or more whitespace");
   else
      return QString();
}
