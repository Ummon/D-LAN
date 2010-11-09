#include <MainWindow.h>
#include <ui_MainWindow.h>
using namespace GUI;

#include <QTabBar>
#include <QMdiSubWindow>
#include <QPainter>
#include <QMenu>

#include <Protos/gui_settings.pb.h>

#include <Common/Settings.h>

#include <StatusBar.h>

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    widgetChat(0),
    widgetDownloads(0),
    widgetUploads(0),
    peerListModel(coreConnection)
{
    this->ui->setupUi(this);

    ui->statusBar->addWidget(new StatusBar(this->coreConnection), 1);

    this->ui->tblPeers->setModel(&this->peerListModel);

    this->ui->tblPeers->setItemDelegate(new PeerTableDelegate());
    this->ui->tblPeers->horizontalHeader()->setResizeMode(0, QHeaderView::Stretch);
    this->ui->tblPeers->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
    this->ui->tblPeers->horizontalHeader()->setVisible(false);

    // TODO : is there an another way to reduce the row size?
    this->ui->tblPeers->verticalHeader()->setResizeMode(QHeaderView::Fixed);
    this->ui->tblPeers->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);

    this->ui->tblPeers->verticalHeader()->setVisible(false);
    this->ui->tblPeers->setSelectionBehavior(QAbstractItemView::SelectRows);
    this->ui->tblPeers->setSelectionMode(QAbstractItemView::SingleSelection);
    this->ui->tblPeers->setShowGrid(false);
    this->ui->tblPeers->setAlternatingRowColors(true);

    this->ui->tblPeers->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(this->ui->tblPeers, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuPeers(const QPoint&)));
    connect(this->ui->tblPeers, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(browse()));

    connect(this->ui->butSearch, SIGNAL(clicked()), this, SLOT(search()));
    connect(this->ui->txtSearch, SIGNAL(returnPressed()), this, SLOT(search()));

    this->addWidgetSettings();

    this->coreDisconnected(); // Initial state.

    connect(&this->coreConnection, SIGNAL(coreConnected()), this, SLOT(coreConnected()));
    connect(&this->coreConnection, SIGNAL(coreDisconnected()), this, SLOT(coreDisconnected()));
    this->coreConnection.connectToCore();
}

MainWindow::~MainWindow()
{
   disconnect(&this->coreConnection, SIGNAL(coreDisconnected()), this, SLOT(coreDisconnected())); // To avoid calling 'coreDisconnected' after deleted 'this->ui'.
   delete this->ui;
}

void MainWindow::coreConnected()
{
   this->addWidgetChat();
   this->addWidgetDownloads();
   this->addWidgetUploads();
   this->widgetSettings->coreConnected();
   this->ui->txtSearch->setDisabled(false);
   this->ui->butSearch->setDisabled(false);
   this->ui->mdiArea->setActiveSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
}

void MainWindow::coreDisconnected()
{
   this->removeWidgetUploads();
   this->removeWidgetDownloads();
   this->removeWidgetChat();
   this->removeAllWidgets();
   this->widgetSettings->coreDisconnected();
   this->ui->txtSearch->setDisabled(true);
   this->ui->butSearch->setDisabled(true);
   this->peerListModel.clear();
}

void MainWindow::displayContextMenuPeers(const QPoint& point)
{
   QMenu menu;
   menu.addAction("Browse", this, SLOT(browse()));
   menu.exec(this->ui->tblPeers->mapToGlobal(point));
}

void MainWindow::browse()
{
   QModelIndex i = this->ui->tblPeers->currentIndex();
   if (i.isValid())
   {
      Common::Hash peerID = this->peerListModel.getPeerID(i.row());
      if (!peerID.isNull())
         this->addWidgetBrowse(peerID);
   }
}

void MainWindow::search()
{
   if (!this->ui->txtSearch->text().isEmpty())
   {
      this->addWidgetSearch(this->ui->txtSearch->text());
   }
}

/**
  * The widget can be a WidgetBrowse or a WidgetSearch.
  */
void MainWindow::removeWidget(QWidget* widget)
{
   WidgetBrowse* widgetBrowse;
   if (widgetBrowse = dynamic_cast<WidgetBrowse*>(widget))
      this->widgetsBrowse.removeOne(widgetBrowse);

   WidgetSearch* widgetSearch;
   if (widgetSearch = dynamic_cast<WidgetSearch*>(widget))
      this->widgetsSearch.removeOne(widgetSearch);

   this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(widget->parent()));
}

void MainWindow::keyPressEvent(QKeyEvent* event)
{
   // CTRL.
   if (event->modifiers().testFlag(Qt::ControlModifier))
   {
      switch (event->key())
      {
      case 'f':
      case 'F':
         this->ui->txtSearch->setFocus();
         this->ui->txtSearch->selectAll();
      }
   }
}

/**
  * Remove and delete a sub window from the MDI area.
  */
void MainWindow::removeMdiSubWindow(QMdiSubWindow* mdiSubWindow)
{
   if (mdiSubWindow)
   {
      // Set a another sub window as active. If we don't do that the windows are all minimised (bug?).
      if (mdiSubWindow == this->ui->mdiArea->currentSubWindow());
      {
         QList<QMdiSubWindow*> subWindows = this->ui->mdiArea->subWindowList();
         if (!subWindows.isEmpty())
         {
            int i = subWindows.indexOf(mdiSubWindow);
            if (i <= 0)
               this->ui->mdiArea->setActiveSubWindow(subWindows[i+1]);
            else
               this->ui->mdiArea->setActiveSubWindow(subWindows[i-1]);
         }
      }

      this->ui->mdiArea->removeSubWindow(mdiSubWindow);
      delete mdiSubWindow;
   }
}

void MainWindow::addWidgetSettings()
{
   this->widgetSettings = new WidgetSettings(this->coreConnection, this);
   this->ui->mdiArea->addSubWindow(this->widgetSettings, Qt::CustomizeWindowHint);
   this->widgetSettings->setWindowState(Qt::WindowMaximized);

   /* To see the close button on the tab.
   foreach (QTabBar* tab, ui->mdiArea->findChildren<QTabBar*>())
   {
      tab->setTabsClosable(true);
   }*/
}

void MainWindow::addWidgetChat()
{
   this->widgetChat = new WidgetChat(this->coreConnection, this->peerListModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetChat, Qt::CustomizeWindowHint);
   //this->mdiChat->setAttribute(Qt::WA_DeleteOnClose);
   this->widgetChat->setWindowState(Qt::WindowMaximized);
}

void MainWindow::removeWidgetChat()
{
   if (this->widgetChat)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetChat->parent()));
      this->widgetChat = 0;
   }
}

void MainWindow::addWidgetDownloads()
{
   this->widgetDownloads = new WidgetDownloads(this->coreConnection, this->peerListModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetDownloads, Qt::CustomizeWindowHint);
   //this->mdiChat->setAttribute(Qt::WA_DeleteOnClose);
   this->widgetDownloads->setWindowState(Qt::WindowMaximized);
}

void MainWindow::removeWidgetDownloads()
{
   if (this->widgetDownloads)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetDownloads->parent()));
      this->widgetDownloads = 0;
   }
}

void MainWindow::addWidgetUploads()
{
   this->widgetUploads = new WidgetUploads(this->coreConnection, this->peerListModel, this);
   this->ui->mdiArea->addSubWindow(this->widgetUploads, Qt::CustomizeWindowHint);
   //this->mdiChat->setAttribute(Qt::WA_DeleteOnClose);
   this->widgetUploads->setWindowState(Qt::WindowMaximized);
}

void MainWindow::removeWidgetUploads()
{
   if (this->widgetUploads)
   {
      this->removeMdiSubWindow(dynamic_cast<QMdiSubWindow*>(this->widgetUploads->parent()));
      this->widgetUploads = 0;
   }
}

void MainWindow::addWidgetBrowse(const Common::Hash& peerID)
{
   // If there is already a browse for the given peer we show it.
   for (QListIterator<WidgetBrowse*> i(this->widgetsBrowse); i.hasNext();)
   {
      WidgetBrowse* widget = i.next();
      if (widget->getPeerID() == peerID)
      {
         this->ui->mdiArea->setActiveSubWindow(static_cast<QMdiSubWindow*>(widget->parent()));
         return;
      }
   }

   WidgetBrowse* widgetBrowse = new WidgetBrowse(this->coreConnection, this->peerListModel, peerID, this);
   this->ui->mdiArea->addSubWindow(widgetBrowse, Qt::CustomizeWindowHint);
   //this->mdiChat->setAttribute(Qt::WA_DeleteOnClose);
   widgetBrowse->setWindowState(Qt::WindowMaximized);
   this->widgetsBrowse << widgetBrowse;

   QTabBar* tab = ui->mdiArea->findChild<QTabBar*>();
   TabCloseButton* closeButton = new TabCloseButton(widgetBrowse);
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));
   tab->setTabButton(tab->count() - 1, QTabBar::RightSide, closeButton);
}

void MainWindow::addWidgetSearch(const QString& term)
{
   WidgetSearch* widgetSearch = new WidgetSearch(this->coreConnection, this->peerListModel, term, this);
   this->ui->mdiArea->addSubWindow(widgetSearch, Qt::CustomizeWindowHint);
   //this->mdiChat->setAttribute(Qt::WA_DeleteOnClose);
   widgetSearch->setWindowState(Qt::WindowMaximized);
   this->widgetsSearch << widgetSearch;

   QTabBar* tab = ui->mdiArea->findChild<QTabBar*>();
   TabCloseButton* closeButton = new TabCloseButton(widgetSearch);
   connect(closeButton, SIGNAL(clicked(QWidget*)), this, SLOT(removeWidget(QWidget*)));
   tab->setTabButton(tab->count() - 1, QTabBar::RightSide, closeButton);
}

void MainWindow::removeAllWidgets()
{
   foreach (WidgetBrowse* widget, this->widgetsBrowse)
      this->removeWidget(widget);

   foreach (WidgetSearch* widget, this->widgetsSearch)
      this->removeWidget(widget);
}

/////

/**
  * Highlight ourself in the peers list.
  */
void PeerTableDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   const PeerListModel* model = static_cast<const PeerListModel*>(index.model());

   QStyleOptionViewItemV4 newOption(option);

   if (model->isOurself(index.row()))
      painter->fillRect(option.rect, QColor(192, 255, 192));

   QItemDelegate::paint(painter, newOption, index);
}

/////

TabCloseButton::TabCloseButton(QWidget* widget)
   : widget(widget)
{
    setFocusPolicy(Qt::NoFocus);
#ifndef QT_NO_CURSOR
    setCursor(Qt::ArrowCursor);
#endif
#ifndef QT_NO_TOOLTIP
    setToolTip(tr("Close Tab"));
#endif
    resize(sizeHint());

    connect (this, SIGNAL(clicked()), this, SLOT(buttonClicked()));
}

QSize TabCloseButton::sizeHint() const
{
    ensurePolished();
    int width = style()->pixelMetric(QStyle::PM_TabCloseIndicatorWidth, 0, this);
    int height = style()->pixelMetric(QStyle::PM_TabCloseIndicatorHeight, 0, this);
    return QSize(width, height);
}

void TabCloseButton::enterEvent(QEvent *event)
{
    if (isEnabled())
        update();
    QAbstractButton::enterEvent(event);
}

void TabCloseButton::leaveEvent(QEvent *event)
{
    if (isEnabled())
        update();
    QAbstractButton::leaveEvent(event);
}

void TabCloseButton::paintEvent(QPaintEvent *)
{
    QPainter p(this);
    QStyleOption opt;
    opt.init(this);
    opt.state |= QStyle::State_AutoRaise;
    if (isEnabled() && underMouse() && !isChecked() && !isDown())
        opt.state |= QStyle::State_Raised;
    if (isChecked())
        opt.state |= QStyle::State_On;
    if (isDown())
        opt.state |= QStyle::State_Sunken;

    if (const QTabBar *tb = qobject_cast<const QTabBar *>(parent())) {
        int index = tb->currentIndex();
        QTabBar::ButtonPosition position = (QTabBar::ButtonPosition)style()->styleHint(QStyle::SH_TabBar_CloseButtonPosition, 0, tb);
        if (tb->tabButton(index, position) == this)
            opt.state |= QStyle::State_Selected;
    }

    style()->drawPrimitive(QStyle::PE_IndicatorTabClose, &opt, &p, this);
}

void TabCloseButton::buttonClicked()
{
   emit clicked(this->widget);
}
