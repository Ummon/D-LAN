#pragma once

#include <limits>

#include <QHash>
#include <QList>

namespace FM
{
   class File;

   // Scheduling only: does not own or dereference files. FileUpdater::mutex protects
   // every access. Times are milliseconds from the updater's monotonic clock.
   class HashingQueue
   {
   public:
      // Ascending: a file waiting in a higher tier is always hashed first.
      enum class Priority
      {
         Normal,
         Hinted, // Files a peer will ask the hashes of soon, see 'IFileManager::prioritizeEntriesToHash(..)'.
         Requested // Files a peer is currently asking the hashes of.
      };

      void enqueue(File* file, qint64 remaining, Priority priority = Priority::Normal)
      {
         if (remaining <= 0)
         {
            this->remove(file);
            return;
         }

         auto it = this->work.find(file);
         if (it == this->work.end())
         {
            this->work.insert(file, Work { remaining, priority, -1 });
            this->list(priority).append(file);
            this->remaining += remaining;
            return;
         }

         this->remaining += remaining - it->remaining;
         it->remaining = remaining;
         if (priority > it->priority)
         {
            // Promotion changes priority, but never bypasses an I/O retry deadline.
            if (it->retryAt < 0)
            {
               this->list(it->priority).removeOne(file);
               this->list(priority).append(file);
            }
            it->priority = priority;
         }
      }

      void finishPass(File* file, qint64 remaining, bool ioError, qint64 now, int retryDelay)
      {
         // Removal during hashing must not resurrect the file.
         if (!this->contains(file))
            return;
         this->enqueue(file, remaining);
         auto it = this->work.find(file);
         if (it == this->work.end())
            return;

         if (ioError)
         {
            this->list(it->priority).removeOne(file);
            if (it->retryAt < 0)
               this->retries.append(file);
            it->retryAt = now + retryDelay;
         }
         else if (it->priority == Priority::Requested && it->retryAt < 0)
         {
            // Share each hashing batch fairly between requests. Hinted files keep the order they were given.
            this->requested.removeOne(file);
            this->requested.append(file);
         }
      }

      void releaseDueRetries(qint64 now)
      {
         for (auto it = this->retries.begin(); it != this->retries.end();)
         {
            Work& job = this->work[*it];
            if (job.retryAt > now)
            {
               ++it;
               continue;
            }
            job.retryAt = -1;
            this->list(job.priority).append(*it);
            it = this->retries.erase(it);
         }
      }

      int retryTimeout(qint64 now) const
      {
         qint64 timeout = -1;
         for (File* file : this->retries)
         {
            const qint64 delay = qMax<qint64>(0, this->work.value(file).retryAt - now);
            timeout = timeout < 0 ? delay : qMin(timeout, delay);
         }
         return static_cast<int>(qMin<qint64>(timeout, std::numeric_limits<int>::max()));
      }

      File* next() const
      {
         for (const QList<File*>* files : { &this->requested, &this->hinted, &this->normal })
            if (!files->isEmpty())
               return files->first();
         return nullptr;
      }

      void remove(File* file)
      {
         auto it = this->work.find(file);
         if (it == this->work.end())
            return;
         this->remaining -= it->remaining;
         if (it->retryAt >= 0)
            this->retries.removeOne(file);
         else
            this->list(it->priority).removeOne(file);
         this->work.erase(it);
      }

      template<typename Predicate> void removeIf(Predicate predicate)
      {
         for (auto it = this->work.begin(); it != this->work.end();)
            if (predicate(it.key()))
            {
               this->remaining -= it->remaining;
               it = this->work.erase(it);
            }
            else
               ++it;
         const auto removed = [this](File* file) { return !this->contains(file); };
         this->normal.removeIf(removed);
         this->hinted.removeIf(removed);
         this->requested.removeIf(removed);
         this->retries.removeIf(removed);
      }

      bool contains(File* file) const { return this->work.contains(file); }
      bool isEmpty() const { return this->work.isEmpty(); }
      qsizetype size() const { return this->work.size(); }
      qint64 remainingBytes() const { return this->remaining; }

   private:
      QList<File*>& list(Priority priority)
      {
         switch (priority)
         {
         case Priority::Requested: return this->requested;
         case Priority::Hinted: return this->hinted;
         case Priority::Normal: break;
         }
         return this->normal;
      }

      struct Work
      {
         qint64 remaining;
         Priority priority;
         qint64 retryAt;
      };
      QHash<File*, Work> work;
      QList<File*> normal;
      QList<File*> hinted;
      QList<File*> requested;
      QList<File*> retries;
      qint64 remaining = 0;
   };
}
