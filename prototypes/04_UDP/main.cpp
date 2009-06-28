#include <QtCore/QCoreApplication>
#include <QtCore/QTextStream>
#include <QtCore/QList>
#include <QtNetwork/QNetworkInterface>
#include <QtNetwork/QUdpSocket>

#include <Chat.h>

int main(int argc, char *argv[])
{  
   QCoreApplication a(argc, argv);
   
   QTextStream out(stdout);   
   QTextStream in(stdin);
   
   out << "Interfaces : " << endl;
   QList<QNetworkInterface> interfaces = QNetworkInterface::allInterfaces();
   foreach (QNetworkInterface interface, interfaces)
   {
      out << interface.name() << " : ";
      
      if (interface.flags() && QNetworkInterface::CanMulticast)
         out << "Multicast enable" << endl;
      else
         out << "Multicast disable" << endl;
   }
   
   Chat chat;
      
   out << "Type something and hit 'return'." << endl <<
         " - blank line : refresh received messages" << endl <<
         " - 'quit' : quit the application" << endl;
   
   QString line;
   for (;;)
   {
      line = in.readLine();
      if (line == "quit")
         return 0;
      if (!line.isEmpty())
         chat.sendMessage(line);
      a.processEvents();
   }
   
   return a.exec();
}
