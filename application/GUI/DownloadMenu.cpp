#include <DownloadMenu.h>
using namespace GUI;

#include <QMenu>
#include <QAction>

DownloadMenu::DownloadMenu(QSharedPointer<RCC::ICoreConnection> coreConnection, const DirListModel& sharedDirsModel) :
   coreConnection(coreConnection), sharedDirsModel(sharedDirsModel)
{
}

void DownloadMenu::show(const QPoint& globalPosition)
{
   QMenu menu;

   for (QListIterator<Common::SharedDir> i(this->sharedDirsModel.getDirs()); i.hasNext();)
   {
      Common::SharedDir sharedDir = i.next();
      QAction* action = new QAction(
         QIcon(":/icons/ressources/download.png"),
         QString("Download selected entries to %1").arg(sharedDir.path),
         &menu
      );
      sharedDir.path = "/"; // A bit dirty, path semantic change, it's now the relative path (not the absolute path).
      action->setData(QVariant::fromValue(sharedDir));
      connect(action, SIGNAL(triggered()), this, SLOT(actionTriggered()));
      menu.addAction(action);
   }

   menu.exec(globalPosition);
}

void DownloadMenu::actionTriggered()
{
   QAction* action = static_cast<QAction*>(this->sender());
   Common::SharedDir sharedDir = action->data().value<Common::SharedDir>();
   emit downloadTo(sharedDir.ID, sharedDir.path);
}
