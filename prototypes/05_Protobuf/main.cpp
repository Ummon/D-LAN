#include <QtCore/QCoreApplication>
#include <QtCore/QFile>
#include <QtCore/QIODevice>
#include <QtCore/QDataStream>
#include <QtCore/QDebug>

#include <addressbook.pb.h>

/**
  * Create a file containing an addressbook.
  */
void createAddressbook(const QString& filename)
{
   QFile f(filename);
   f.open(QIODevice::WriteOnly);  
   
   tutorial::AddressBook addressBook;
   tutorial::Person* p1 = addressBook.add_person();
   p1->set_id(23);
   p1->set_name("Jean-Pierre");
   p1->set_email("jp@gmail.com");
   tutorial::Person::PhoneNumber* phone1 = p1->add_phone();
   phone1->set_number("023 432 32 09");
   phone1->set_type(tutorial::Person::HOME);
   tutorial::Person* p2 = addressBook.add_person();
   p2->set_id(34);
   p2->set_name("Paul");
   p2->set_email("paul@gmail.com");
   
   addressBook.SerializeToFileDescriptor(f.handle());
}

/**
  * Read a file which contains an addressbook.
  */
void readAddressbook(const QString& filename)
{
   QFile f(filename);
   f.open(QIODevice::ReadOnly);  
   
   tutorial::AddressBook addressBook;
   addressBook.ParseFromFileDescriptor(f.handle());
   
   QString serializedText(addressBook.DebugString().c_str());
   qDebug() << serializedText;
}

/**
  * See : http://code.google.com/apis/protocolbuffers/docs/cpptutorial.html
  */
int main(int argc, char *argv[])
{
   GOOGLE_PROTOBUF_VERIFY_VERSION;
   
   const QString filename("addressbook.bin");
   createAddressbook(filename);
   readAddressbook(filename);
}
