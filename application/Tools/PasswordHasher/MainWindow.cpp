#include <MainWindow.h>
using namespace PasswordHasher;

#include <ui_MainWindow.h>

#include <Protos/common.pb.h>
#include <google/protobuf/text_format.h>

#include <Common/Hash.h>

MainWindow::MainWindow(QWidget *parent)
   : QMainWindow(parent), ui(new Ui::MainWindow)
{
   ui->setupUi(this);
   this->computeHash();

   connect(this->ui->txtPass1, SIGNAL(textChanged(const QString&)), this, SLOT(computeHash()));
   connect(this->ui->txtPass2, SIGNAL(textChanged(const QString&)), this, SLOT(computeHash()));
}

MainWindow::~MainWindow()
{
   delete ui;
}

void MainWindow::computeHash()
{
   if (this->ui->txtPass1->text() != this->ui->txtPass2->text())
   {
      this->ui->txtResult->setText("Passwords aren't the same");
   }
   else if (this->ui->txtPass1->text().isEmpty())
   {
      this->ui->txtResult->setText("Passwords are empty");
   }
   else
   {
      Common::Hash hash = Common::Hasher::hashWithSalt(this->ui->txtPass1->text());

      Protos::Common::Hash hashMessage;
      const google::protobuf::FieldDescriptor* hashField = hashMessage.GetDescriptor()->FindFieldByName("hash");

      std::string encodedHash;
      hashMessage.set_hash(hash.getData(), Common::Hash::HASH_SIZE);
      google::protobuf::TextFormat::PrintFieldValueToString(hashMessage, hashField, -1, &encodedHash);

      this->ui->txtResult->setText(QString::fromStdString(encodedHash));
   }
}
