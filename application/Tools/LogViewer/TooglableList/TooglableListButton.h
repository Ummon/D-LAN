#ifndef TOOGLABLELISTBUTTON_H
#define TOOGLABLELISTBUTTON_H

#include <QEvent>
#include <QPushButton>

class TooglableListButton : public QPushButton
{
   Q_OBJECT
public:
    TooglableListButton(QWidget* parent = 0);

protected:
   bool event(QEvent* event);

signals:
    void rightClicked();
};

#endif
