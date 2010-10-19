/********************************************************************************
** Form generated from reading UI file 'MainWindow.ui'
**
** Created: Tue Oct 19 02:11:00 2010
**      by: Qt User Interface Compiler version 4.7.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLineEdit>
#include <QtGui/QMainWindow>
#include <QtGui/QMdiArea>
#include <QtGui/QPushButton>
#include <QtGui/QStatusBar>
#include <QtGui/QTableView>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QWidget *centralWidget;
    QHBoxLayout *layMain;
    QVBoxLayout *layLeftPanel;
    QHBoxLayout *laySearch;
    QLineEdit *txtSearch;
    QPushButton *butSearch;
    QTableView *tblPeers;
    QMdiArea *mdiArea;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(724, 423);
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        layMain = new QHBoxLayout(centralWidget);
        layMain->setSpacing(6);
        layMain->setContentsMargins(6, 6, 6, 6);
        layMain->setObjectName(QString::fromUtf8("layMain"));
        layLeftPanel = new QVBoxLayout();
        layLeftPanel->setSpacing(6);
        layLeftPanel->setObjectName(QString::fromUtf8("layLeftPanel"));
        laySearch = new QHBoxLayout();
        laySearch->setSpacing(0);
        laySearch->setObjectName(QString::fromUtf8("laySearch"));
        txtSearch = new QLineEdit(centralWidget);
        txtSearch->setObjectName(QString::fromUtf8("txtSearch"));
        QSizePolicy sizePolicy(QSizePolicy::Maximum, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(txtSearch->sizePolicy().hasHeightForWidth());
        txtSearch->setSizePolicy(sizePolicy);

        laySearch->addWidget(txtSearch);

        butSearch = new QPushButton(centralWidget);
        butSearch->setObjectName(QString::fromUtf8("butSearch"));
        QSizePolicy sizePolicy1(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(1);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(butSearch->sizePolicy().hasHeightForWidth());
        butSearch->setSizePolicy(sizePolicy1);
        butSearch->setMinimumSize(QSize(25, 0));
        butSearch->setAutoFillBackground(false);
        QIcon icon;
        icon.addFile(QString::fromUtf8(":/icons/ressources/zoom.png"), QSize(), QIcon::Normal, QIcon::Off);
        butSearch->setIcon(icon);
        butSearch->setFlat(true);

        laySearch->addWidget(butSearch);


        layLeftPanel->addLayout(laySearch);

        tblPeers = new QTableView(centralWidget);
        tblPeers->setObjectName(QString::fromUtf8("tblPeers"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(tblPeers->sizePolicy().hasHeightForWidth());
        tblPeers->setSizePolicy(sizePolicy2);
        tblPeers->setMinimumSize(QSize(150, 0));

        layLeftPanel->addWidget(tblPeers);


        layMain->addLayout(layLeftPanel);

        mdiArea = new QMdiArea(centralWidget);
        mdiArea->setObjectName(QString::fromUtf8("mdiArea"));
        mdiArea->setLineWidth(0);
        mdiArea->setViewMode(QMdiArea::TabbedView);

        layMain->addWidget(mdiArea);

        MainWindow->setCentralWidget(centralWidget);
        statusBar = new QStatusBar(MainWindow);
        statusBar->setObjectName(QString::fromUtf8("statusBar"));
        MainWindow->setStatusBar(statusBar);

        retranslateUi(MainWindow);

        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "Aybabtu", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        butSearch->setToolTip(QApplication::translate("MainWindow", "Search", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        butSearch->setText(QString());
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
