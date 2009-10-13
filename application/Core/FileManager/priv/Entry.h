#ifndef ENTRY_H
#define ENTRY_H

#include <QString>

class Entry
{
public:
   Entry(const QString& name);
   virtual ~Entry() {};

   /**
     * Return the full absolute path to the entry.
     */
   virtual QString getPath() = 0;

   virtual qint64 getSize();

protected:
   QString name;
   qint64 size;
};

#endif // ENTRY_H
