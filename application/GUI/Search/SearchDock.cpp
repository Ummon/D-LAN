#include <Search/SearchDock.h>
#include <ui_SearchDock.h>
using namespace GUI;

#include <QKeyEvent>

SearchDock::SearchDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QDockWidget(parent),
   ui(new Ui::SearchDock),
   coreConnection(coreConnection)
{
   this->ui->setupUi(this);

#ifdef Q_OS_DARWIN
   this->ui->butSearch->setMaximumWidth(24);
   this->ui->butSearchOwnFiles->setMaximumWidth(24);
#endif

   connect(this->ui->butSearch, SIGNAL(clicked()), this, SLOT(searchOtherPeers()));
   connect(this->ui->butSearchOwnFiles, SIGNAL(clicked()), this, SLOT(searchOwnFiles()));
   this->ui->txtSearch->installEventFilter(this); // the signal 'returnPressed()' doesn't contain the key modifier information (shift = search among our files), we have to use a event filter.

   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->coreDisconnected(false); // Initial state.
}

SearchDock::~SearchDock()
{
   delete this->ui;
}

void SearchDock::setFocusToLineEdit()
{
   this->ui->txtSearch->setFocus();
   this->ui->txtSearch->selectAll();
}

bool SearchDock::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->ui->txtSearch && event->type() == QEvent::KeyPress && static_cast<QKeyEvent*>(event)->key() == Qt::Key_Return)
   {
      if (static_cast<QKeyEvent*>(event)->modifiers().testFlag(Qt::ShiftModifier))
         this->searchOwnFiles();
      else
         this->searchOtherPeers();
   }

   return QDockWidget::eventFilter(obj, event);
}

void SearchDock::coreConnected()
{
   this->ui->txtSearch->setDisabled(false);
   this->ui->butSearch->setDisabled(false);
   this->ui->butSearchOwnFiles->setDisabled(false);
}

void SearchDock::coreDisconnected(bool force)
{
   this->ui->txtSearch->setDisabled(true);
   this->ui->butSearch->setDisabled(true);
   this->ui->butSearchOwnFiles->setDisabled(true);
}

void SearchDock::searchOtherPeers()
{
   this->search(false);
}

void SearchDock::searchOwnFiles()
{
   this->search(true);
}

void SearchDock::search(bool ownFiles)
{
   this->ui->txtSearch->setText(this->ui->txtSearch->text().trimmed());

   if (!this->ui->txtSearch->text().isEmpty())
      emit search(this->ui->txtSearch->text(), ownFiles);
}
