/*
  ==============================================================================

   This file is part of the juce_core module of the JUCE library.
   Copyright (c) 2015 - ROLI Ltd.

   Permission to use, copy, modify, and/or distribute this software for any purpose with
   or without fee is hereby granted, provided that the above copyright notice and this
   permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD
   TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN
   NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
   DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
   IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

   ------------------------------------------------------------------------------

   NOTE! This permissive ISC license applies ONLY to files within the juce_core module!
   All other JUCE modules are covered by a dual GPL/commercial license, so if you are
   using any other modules, be sure to check that you also comply with their license.

   For more details, visit www.juce.com

  ==============================================================================
*/

#ifndef JUCE_CRITICALSECTION_H_INCLUDED
#define JUCE_CRITICALSECTION_H_INCLUDED


//==============================================================================
/**
    A re-entrant mutex.

    A CriticalSection acts as a re-entrant mutex object. The best way to lock and unlock
    one of these is by using RAII in the form of a local ScopedLock object - have a look
    through the codebase for many examples of how to do this.

    In almost all cases you'll want to declare your CriticalSection as a member variable.
    Occasionally you may want to declare one as a static variable, but in that case the usual
    C++ static object order-of-construction warnings should be heeded.

    @see ScopedLock, ScopedTryLock, ScopedUnlock, SpinLock, ReadWriteLock, Thread, InterProcessLock
*/
class JUCE_API  CriticalSection
{
public:
    //==============================================================================
    /** Creates a CriticalSection object. */
    CriticalSection() noexcept;

    /** Destructor.
        If the critical section is deleted whilst locked, any subsequent behaviour
        is unpredictable.
    */
    ~CriticalSection() noexcept;

    //==============================================================================
    /** Acquires the lock.

        If the lock is already held by the caller thread, the method returns immediately.
        If the lock is currently held by another thread, this will wait until it becomes free.

        It's strongly recommended that you never call this method directly - instead use the
        ScopedLock class to manage the locking using an RAII pattern instead.

        @see exit, tryEnter, ScopedLock
    */
    void enter() const noexcept;

    /** Attempts to lock this critical section without blocking.

        This method behaves identically to CriticalSection::enter, except that the caller thread
        does not wait if the lock is currently held by another thread but returns false immediately.

        @return false if the lock is currently held by another thread, true otherwise.
        @see enter
    */
    bool tryEnter() const noexcept;

    /** Releases the lock.

        If the caller thread hasn't got the lock, this can have unpredictable results.

        If the enter() method has been called multiple times by the thread, each
        call must be matched by a call to exit() before other threads will be allowed
        to take over the lock.

        @see enter, ScopedLock
    */
    void exit() const noexcept;


    //==============================================================================
    /** Provides the type of scoped lock to use with a CriticalSection. */
    typedef GenericScopedLock <CriticalSection>       ScopedLockType;

    /** Provides the type of scoped unlocker to use with a CriticalSection. */
    typedef GenericScopedUnlock <CriticalSection>     ScopedUnlockType;

    /** Provides the type of scoped try-locker to use with a CriticalSection. */
    typedef GenericScopedTryLock <CriticalSection>    ScopedTryLockType;


private:
    //==============================================================================
   #if JUCE_WINDOWS
    // To avoid including windows.h in the public JUCE headers, we'll just allocate
    // a block of memory here that's big enough to be used internally as a windows
    // CRITICAL_SECTION structure.
    #if JUCE_64BIT
     uint8 lock[44];
    #else
     uint8 lock[24];
    #endif
   #else
    mutable pthread_mutex_t lock;
   #endif

    JUCE_DECLARE_NON_COPYABLE (CriticalSection)
};


//==============================================================================
/**
    A class that can be used in place of a real CriticalSection object, but which
    doesn't perform any locking.

    This is currently used by some templated classes, and most compilers should
    manage to optimise it out of existence.

    @see CriticalSection, Array, OwnedArray, ReferenceCountedArray
*/
class JUCE_API  DummyCriticalSection
{
public:
    inline DummyCriticalSection() noexcept      {}
    inline ~DummyCriticalSection() noexcept     {}

    inline void enter() const noexcept          {}
    inline bool tryEnter() const noexcept       { return true; }
    inline void exit() const noexcept           {}

    //==============================================================================
    /** A dummy scoped-lock type to use with a dummy critical section. */
    struct ScopedLockType
    {
        ScopedLockType (const DummyCriticalSection&) noexcept {}
    };

    /** A dummy scoped-unlocker type to use with a dummy critical section. */
    typedef ScopedLockType ScopedUnlockType;

private:
    JUCE_DECLARE_NON_COPYABLE (DummyCriticalSection)
};

//==============================================================================
/**
    Automatically locks and unlocks a CriticalSection object.

    You can use a ScopedLock as a local variable to provide RAII-based locking of a CriticalSection.

    e.g. @code

    struct MyObject
    {
        CriticalSection objectLock;

        // assuming that this example function will be called by multiple threads
        void foo()
        {
            const ScopedLock myScopedLock (objectLock);

            // objectLock is now locked..

            ...do some thread-safe work here...

            // ..and objectLock gets unlocked here, as myScopedLock goes out of
            // scope at the end of the block
        }
    };
    @endcode

    @see CriticalSection, ScopedUnlock
*/
typedef CriticalSection::ScopedLockType  ScopedLock;

//==============================================================================
/**
    Automatically unlocks and re-locks a CriticalSection object.

    This is the reverse of a ScopedLock object - instead of locking the critical
    section for the lifetime of this object, it unlocks it.

    Make sure you don't try to unlock critical sections that aren't actually locked!

    e.g. @code

    struct MyObject
    {
        CriticalSection objectLock;

        void foo()
        {
            {
                const ScopedLock myScopedLock (objectLock);

                // objectLock is now locked..

                {
                    ScopedUnlock myUnlocker (objectLock);

                    // ..and now unlocked..
                }

                // ..and now locked again..
            }

            // ..and finally unlocked.
        }
    };
    @endcode

    @see CriticalSection, ScopedLock
*/
typedef CriticalSection::ScopedUnlockType  ScopedUnlock;

//==============================================================================
/**
    Automatically tries to lock and unlock a CriticalSection object.

    Use one of these as a local variable to control access to a CriticalSection.

    e.g. @code

    struct MyObject
    {
        CriticalSection objectLock;

        void foo()
        {
            const ScopedTryLock myScopedTryLock (objectLock);

            // Unlike using a ScopedLock, this may fail to actually get the lock, so you
            // must call the isLocked() method before making any assumptions..
            if (myScopedTryLock.isLocked())
            {
               ...safely do some work...
            }
            else
            {
                // If we get here, then our attempt at locking failed because another thread had already locked it..
            }
        }
    };
    @endcode

    @see CriticalSection::tryEnter, ScopedLock, ScopedUnlock, ScopedReadLock
*/
typedef CriticalSection::ScopedTryLockType  ScopedTryLock;


#endif   // JUCE_CRITICALSECTION_H_INCLUDED
