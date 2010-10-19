/********************************************************************************
** Form generated from reading UI file 'WidgetChat.ui'
**
** Created: Tue Oct 19 01:58:25 2010
**      by: Qt User Interface Compiler version 4.7.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_WIDGETCHAT_H
#define UI_WIDGETCHAT_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QTableView>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_WidgetChat
{
public:
    QVBoxLayout *layMain;
    QTableView *tblChat;
    QHBoxLayout *layMessage;
    QLineEdit *txtMessage;
    QPushButton *butSend;

    void setupUi(QWidget *WidgetChat)
    {
        if (WidgetChat->objectName().isEmpty())
            WidgetChat->setObjectName(QString::fromUtf8("WidgetChat"));
        WidgetChat->resize(400, 314);
        layMain = new QVBoxLayout(WidgetChat);
        layMain->setObjectName(QString::fromUtf8("layMain"));
        layMain->setContentsMargins(0, 8, 0, 0);
        tblChat = new QTableView(WidgetChat);
        tblChat->setObjectName(QString::fromUtf8("tblChat"));

        layMain->addWidget(tblChat);

        layMessage = new QHBoxLayout();
        layMessage->setSpacing(6);
        layMessage->setObjectName(QString::fromUtf8("layMessage"));
        txtMessage = new QLineEdit(WidgetChat);
        txtMessage->setObjectName(QString::fromUtf8("txtMessage"));

        layMessage->addWidget(txtMessage);

        butSend = new QPushButton(WidgetChat);
        butSend->setObjectName(QString::fromUtf8("butSend"));

        layMessage->addWidget(butSend);


        layMain->addLayout(layMessage);


        retranslateUi(WidgetChat);

        QMetaObject::connectSlotsByName(WidgetChat);
    } // setupUi

    void retranslateUi(QWidget *WidgetChat)
    {
        WidgetChat->setWindowTitle(QApplication::translate("WidgetChat", "Chat", 0, QApplication::UnicodeUTF8));
        butSend->setText(QApplication::translate("WidgetChat", "Send", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class WidgetChat: public Ui_WidgetChat {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_WIDGETCHAT_H
