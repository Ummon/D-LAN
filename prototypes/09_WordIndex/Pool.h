#ifndef __POOL_H__
#define __POOL_H__

// CPool:
// An optimised way of performing dynamic memory allocation, using a
// preallocated pool which grows in large blocks as more objects are required.

// To use it, add a static member variable to the class definition:
//	 static CPool m_pool;
// Which has to be initialised in the class's .cpp file as:
//   CPool CItem::m_pool(1000, sizeof(CItem));
//     Where 1000 is the initial number of items in the pool.

// Then add overrides for the new and delete operator of your class:
//	 static void* operator new(const size_t size)
//   {
//       return m_pool.New(size);
//   }
//	 static void operator delete(void* pObject);
//   {
//       m_pool.Delete(pObject);
//   }

// This class was originally implemented as a template but I decided that it
// wasn't necessary since the new and delete operators always use void* anyway.

class CPool
{
public:
	CPool(const int nPoolSize, const size_t nItemSize, const bool bGrow = true);
	~CPool();
	
	void* New(const size_t size);
	void Delete(void* pVoid);
	void Purge();

private:
	CPool(CPool* const pPrev);

	// New allocation pools are created and inserted into a doubly-linked list.
	CPool* const	m_pPrev;
	CPool*			m_pNext;

	const int		m_nPoolSize;	// Maximum number of items in the pool.
	const size_t	m_nItemSize;	// The size of the contained item.
	char*			m_pAvailable;	// Next available item.
	char*			m_pLast;		// End of the pool.
	int			m_nTOS;			// Top of the free stack.
	bool			m_bGrow;		// True if the pool is allowed to grow.

	char*			m_pPool;		// The allocation pool of items.
	char**			m_pFreeStack;	// The stack of deleted items.
};

#endif
