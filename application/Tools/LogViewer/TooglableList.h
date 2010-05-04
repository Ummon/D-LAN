#ifndef TOOGLABLELIST_H
#define TOOGLABLELIST_H

#include <QWidget>

namespace Ui { class TooglableList; }

struct ToogleState
{
   ToogleState(bool pushed, QString str) : pushed(pushed), str(str) {}
   bool pushed;
   QString str;
};

class TooglableList : public QWidget
{
   Q_OBJECT
public:
   TooglableList(QWidget* parent = 0);
   ~TooglableList();

   void setList(const QStringList& list);
   QList<ToogleState> getList();

signals:
   void stateChanged();

private:
   Ui::TooglableList* ui;
};

#endif
