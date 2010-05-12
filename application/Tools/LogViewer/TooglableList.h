#ifndef TOOGLABLELIST_H
#define TOOGLABLELIST_H

#include <QWidget>

namespace Ui { class TooglableList; }

class TooglableList : public QWidget
{
   Q_OBJECT
public:
   TooglableList(QWidget* parent = 0);
   ~TooglableList();

   void setList(const QStringList& list);
   QStringList getList();

signals:
   void stateChanged();

public slots:
   void addItem(const QString& item);
   void checkAll();

private slots:
   void butToogled(bool);

private:
   void clear();

   bool disableSignalStateChanged; ///< When all buttons are checked ('checkAll') it's usefull to avoid multiple signal sent.

   Ui::TooglableList* ui;
};

#endif
