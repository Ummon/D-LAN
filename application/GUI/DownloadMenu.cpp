/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <DownloadMenu.h>
using namespace GUI;

#include <algorithm>

#include <QAction>
#include <QCollator>
#include <QPointer>

#include <Utils.h>

namespace
{
   // Beyond this number the remaining sub-folders are not listed, the user can still choose one with 'Download selected items to . . .'.
   const int MAX_NB_SUB_FOLDERS = 100;

   // '&' is used by the menus to define a mnemonic.
   QString escapeMenuText(QString text)
   {
      return text.replace('&', "&&");
   }
}

/**
  * @class GUI::DownloadMenu
  *
  * Show the list of shared directory as a menu.
  * - The menu can be shown by calling 'show(..)'.
  * - Each shared directory is a sub-menu whose folders can be browsed through sub-menus, they are fetched from the core when shown.
  * - When the user select an action, the signal 'downloadTo(..)' is emitted.
  * - Can be sub-classed to add some entries. In this case 'onShowMenu(..)' must be overridden.
  */

DownloadMenu::DownloadMenu(QSharedPointer<RCC::ICoreConnection> coreConnection, const SharedEntryListModel& sharedEntryListModel) :
   coreConnection(coreConnection),
   sharedEntryListModel(sharedEntryListModel)
{
}

void DownloadMenu::show(const QPoint& globalPosition)
{
   QMenu menu;

   const QList<Common::SharedEntry>& sharedDirs = this->sharedEntryListModel.getSharedDirectories();

   if (!sharedDirs.isEmpty())
   {
      QAction* actionDownload = new QAction(
         QIcon(":/icons/resources/download.svg"),
         tr("Download selected items to the first directory folder with enough free space"),
         &menu
      );
      connect(actionDownload, &QAction::triggered, this, &DownloadMenu::download);
      menu.addAction(actionDownload);
   }

   for (const auto& sharedDir : sharedDirs)
   {
      // A shared directory is a folder with an empty path and an empty name.
      Protos::Common::Entry root;
      root.set_type(Protos::Common::Entry::DIR);
      root.mutable_shared_entry()->mutable_id()->set_hash(sharedDir.ID.getData(), Common::Hash::HASH_SIZE);

      QMenu* sharedDirMenu = this->createFolderMenu(
         QString(tr("Download selected items to %1")).arg(escapeMenuText(sharedDir.path.toString())),
         sharedDir.ID,
         QStringList(),
         root,
         &menu
      );
      sharedDirMenu->setIcon(QIcon(":/icons/resources/download.svg"));
      menu.addMenu(sharedDirMenu);
   }

   QAction* actionChooseAndDownload = new QAction(
      QIcon(":/icons/resources/download.svg"),
      tr("Download selected items to . . ."),
      &menu
   );
   connect(actionChooseAndDownload, &QAction::triggered, this, qOverload<>(&DownloadMenu::downloadTo));
   menu.addAction(actionChooseAndDownload);

   this->onShowMenu(menu);

   // The widget owning this object can be deleted while its menu is shown, for example if the connection to the
   // core is lost: nothing of this object may then be accessed.
   const QPointer<DownloadMenu> self(this);
   menu.exec(globalPosition);
   if (!self)
      return;

   // The menus waiting for these results no longer exist.
   this->browseResults.clear();
}

/**
  * Create a menu to download into 'folder', its sub-folders are loaded the first time the menu is shown.
  * @param relativeDirs The path of 'folder' relative to its shared directory.
  */
QMenu* DownloadMenu::createFolderMenu(
   const QString& title,
   const Common::Hash& sharedDirID,
   const QStringList& relativeDirs,
   const Protos::Common::Entry& folder,
   QWidget* parent
)
{
   QMenu* menu = new QMenu(title, parent);
   menu->setIcon(QIcon(":/icons/resources/folder.svg"));

   QAction* actionDownloadHere = menu->addAction(QIcon(":/icons/resources/download.svg"), tr("Download here"));
   connect(actionDownloadHere, &QAction::triggered, this, [this, sharedDirID, relativeDirs] {
      emit downloadTo(sharedDirID, Common::Path(relativeDirs));
   });

   if (!folder.is_empty())
   {
      QAction* actionLoading = menu->addAction(tr("Loading . . ."));
      actionLoading->setEnabled(false);
      connect(
         menu,
         &QMenu::aboutToShow,
         this,
         [this, menu, actionLoading, sharedDirID, relativeDirs, folder] {
            this->loadSubFolders(menu, actionLoading, sharedDirID, relativeDirs, folder);
         },
         Qt::SingleShotConnection
      );
   }

   return menu;
}

/**
  * Ask the core for the sub-folders of 'folder' and add them to 'menu' in place of 'actionLoading'.
  */
void DownloadMenu::loadSubFolders(
   QMenu* menu,
   QAction* actionLoading,
   const Common::Hash& sharedDirID,
   const QStringList& relativeDirs,
   const Protos::Common::Entry& folder
)
{
   const QSharedPointer<RCC::IBrowseResult> browseResult = this->coreConnection->browse(this->coreConnection->getRemoteID(), folder);
   this->browseResults << browseResult;

   const QPointer<QAction> loading(actionLoading);

   connect(browseResult.data(), &RCC::IBrowseResult::result, menu,
      [this, menu, loading, sharedDirID, relativeDirs](const google::protobuf::RepeatedPtrField<Protos::Common::Entries>& result)
      {
         delete loading.data();

         QList<QPair<QString, const Protos::Common::Entry*>> subFolders;
         if (!result.empty())
            for (const auto& entry : result.Get(0).entries())
               if (entry.type() == Protos::Common::Entry::DIR)
                  subFolders << qMakePair(QString::fromStdString(entry.name()), &entry);

         if (subFolders.isEmpty())
            return;

         QCollator collator;
         collator.setNumericMode(true);
         collator.setCaseSensitivity(Qt::CaseInsensitive);
         std::sort(subFolders.begin(), subFolders.end(), [&collator](const auto& a, const auto& b) { return collator.compare(a.first, b.first) < 0; });

         menu->addSeparator();

         for (int i = 0; i < subFolders.size() && i < MAX_NB_SUB_FOLDERS; i++)
            menu->addMenu(this->createFolderMenu(
               escapeMenuText(subFolders[i].first),
               sharedDirID,
               relativeDirs + QStringList { subFolders[i].first },
               *subFolders[i].second,
               menu
            ));

         if (subFolders.size() > MAX_NB_SUB_FOLDERS)
         {
            QAction* actionMore = menu->addAction(tr("%1 more folders . . .").arg(subFolders.size() - MAX_NB_SUB_FOLDERS));
            connect(actionMore, &QAction::triggered, this, qOverload<>(&DownloadMenu::downloadTo));
         }
      }
   );

   connect(browseResult.data(), &Common::Timeoutable::timeout, menu, [loading] {
      if (loading)
         loading->setText(DownloadMenu::tr("Unable to get the folders"));
   });

   browseResult->start();
}
