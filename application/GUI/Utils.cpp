#include <Utils.h>
using namespace GUI;

#include <QListView>
#include <QFileDialog>
#include <QGridLayout>
#include <QLabel>

#include <Settings/RemoteFileDialog.h>

/**
  * Ask the user to choose one or more directories.
  * TODO : browse the remotes directories (Core) not the local ones.
  */
QStringList Utils::askForDirectories(QSharedPointer<RCC::ICoreConnection> coreConnection, const QString& message)
{
   if (coreConnection->isLocal())
   {
      QFileDialog fileDialog(0, "Choose a directory");
      fileDialog.setOption(QFileDialog::DontUseNativeDialog, true);
      fileDialog.setFileMode(QFileDialog::Directory);

      if (!message.isNull())
      {
         QGridLayout* layout = fileDialog.findChild<QGridLayout*>();
         QLabel* label = new QLabel(message, &fileDialog);
         layout->addWidget(label, layout->rowCount(), 0, 1, -1, Qt::AlignLeft | Qt::AlignVCenter);
      }

      QListView* l = fileDialog.findChild<QListView*>("listView");
      if (l)
         l->setSelectionMode(QAbstractItemView::ExtendedSelection);

      if (fileDialog.exec())
      {
         return fileDialog.selectedFiles();
      }
      return QStringList();
   }
   else
   {
      RemoteFileDialog fileDialog;
      fileDialog.setWindowTitle("Remote folder");
      fileDialog.setText("Remote folder to share : ");
      if (fileDialog.exec())
      {
         return QStringList() << fileDialog.getFolder();
      }
      return QStringList();
   }
}

QStringList Utils::askForDirectoriesToDownloadTo(QSharedPointer<RCC::ICoreConnection> coreConnection)
{
   return Utils::askForDirectories(coreConnection, "<img src=\":/icons/ressources/information.png\" /> <strong>" + QObject::tr("The choosen directory will be shared") + "</strong>");
}
