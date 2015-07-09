/*
 * =============================================================================
 *  Copyright(c) 2011-2013 Organic Motion, Inc. All Rights Reserved.
 *
 *  The coded instructions, statements, computer programs, and/or related
 *  material (collectively the "Code") in these files contain unpublished
 *  information proprietary to Organic Motion, Inc., which is protected by
 *  United States of America federal copyright law and by international
 *  treaties. Use, duplication, and or distribution only with written
 *  permission from Organic Motion, Inc.
 *
 *  THE CODE IS PROVIDED "AS IS" AND WITHOUT WARRANTY.
 * =============================================================================
 */
#ifndef OM_SDK2_OBJECT_H
#define OM_SDK2_OBJECT_H

#include <stdlib.h>

// The OMCALL macro is used to ensure that interface methods always have the
// same calling convention.  If it is not specified, code built with compilers
// other than Visual C++ may crash when interface methods are called.
#if defined(WIN32) || defined(_WIN32)
#  define OMCALL __stdcall
#else
#  define OMCALL
#endif

namespace om
{
namespace sdk2
{

/// <summary>Base interface for all objects provided by the OpenStage SDK.</summary>
class IOMObject
{
public:
  /// <summary>Increment the object's reference count.</summary>
  ///
  /// <returns>The new reference count.</returns>
  virtual unsigned long OMCALL AddRef() const = 0;

  /// <summary>Decrement the object's reference count, deleting the object if
  /// the reference count reaches zero.</summary>
  ///
  /// <returns>The new reference count.</returns>
  virtual unsigned long OMCALL Release() const = 0;
};

/// <summary>Smart pointer to automatically manage the reference count of an
///   object implementing <see cref="IOMObject" />.
/// </summary>
template<typename T>
class OMPtr
{
public:
  OMPtr();

  /// <summary>Construct a smart pointer from a raw pointer.</summary>
  ///
  /// <remarks><para>The constructor takes ownership of the pointer.  Do not
  ///   pass a pointer to multiple OMPtr&lt;T&gt; or else the reference count
  ///   will become corrupted.
  /// </para></remarks>
  ///
  /// <param name="object">A raw pointer to an object.</param>
  explicit OMPtr(T* object);

  /// <summary>Construct a smart pointer from another smart pointer.</summary>
  ///
  /// <remarks><para>The reference count of the object will be incremented.</para></remarks>
  ///
  /// <param name="other">The other smart pointer.</param>
  /**@{*/
  OMPtr(const OMPtr& other);
  template<typename U> OMPtr(const OMPtr<U>& other);
  /**@}*/

#if defined(_MSC_VER) && _MSC_VER >= 1600
  OMPtr(OMPtr&& other);
  template<typename U> OMPtr(OMPtr<U>&& other);
#endif

  ~OMPtr();

  /// <summary>Set this smart pointer to a given raw pointer.</summary>
  ///
  /// <remarks><para>The assignment takes ownership of the pointer.  Do not
  ///   assign a pointer to multiple OMPtr&lt;T&gt; or else the reference count
  ///   will become corrupted.
  /// </para></remarks>
  ///
  /// <param name="object">A raw pointer.</param>
  ///
  /// <returns>A reference to this object.</returns>
  OMPtr& operator=(T* object);

  /// <summary>Set this smart pointer to point to the same object as another
  ///   smart pointer.
  /// </summary>
  ///
  /// <remarks><para>The reference count of the object will be incremented.</para></remarks>
  ///
  /// <param name="other">The other smart pointer.</param>
  ///
  /// <returns>A reference to this object.</returns>
  /**@{*/
  OMPtr& operator=(const OMPtr& other);
  template<typename U> OMPtr<T>& operator=(const OMPtr<U>& other);
  /**@}*/

  /// <summary>Indirect member access operator.</summary>
  ///
  /// <remarks><para>This operator will cause a crash if it is used when the
  ///   pointer is invalid.
  /// </para></remarks>
  ///
  /// <returns>Raw pointer to the object referenced by this smart pointer.</returns>
  T* operator->() const;

  /// <summary>Dereferencing operator.</summary>
  ///
  /// <remarks><para>This operator will cause a crash if it is used when the
  ///   pointer is invalid.
  /// </para></remarks>
  ///
  /// <returns>The result of the operation.</returns>
  T& operator*() const;

  /// <summary>Query if this object is valid.</summary>
  ///
  /// <returns>True if it is valid or false if it is not.</returns>
  bool IsValid() const;

  /// <summary>Get the pointer.</summary>
  ///
  /// <returns>The pointer.</returns>
  T* GetPtr() const;

  /// <summary>Release the reference to the object and invalidate the pointer.</summary>
  void Clear();

  /// <summary>Cast the pointer to a different type.</summary>
  ///
  /// <returns>A new smart pointer of the specified type.</returns>
  template<typename U>
  OMPtr<U> Cast() const;

  /// <summary>Check if a smart pointer is equal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The smart pointer.</param>
  /// <param name="rhs">The raw pointer.</param>
  ///
  /// <returns>True if the pointers are equal.</returns>
  friend bool operator==(const OMPtr& lhs, const T* rhs)
  {
    return lhs.m_object == rhs;
  }

  /// <summary>Check if a smart pointer is equal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The raw pointer.</param>
  /// <param name="rhs">The smart pointer.</param>
  ///
  /// <returns>True if the pointers are equal.</returns>
  friend bool operator==(const T* lhs, const OMPtr& rhs)
  {
    return lhs == rhs.m_object;
  }

  /// <summary>Check if a smart pointer is unequal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The smart pointer.</param>
  /// <param name="rhs">The raw pointer.</param>
  ///
  /// <returns>True if the pointers are unequal.</returns>
  friend bool operator!=(const OMPtr& lhs, const T* rhs)
  {
    return lhs.m_object != rhs;
  }

  /// <summary>Check if a smart pointer is unequal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The raw pointer.</param>
  /// <param name="rhs">The smart pointer.</param>
  ///
  /// <returns>True if the pointers are unequal.</returns>
  friend bool operator!=(const T* lhs, const OMPtr& rhs)
  {
    return lhs != rhs.m_object;
  }

  /// <summary>Check if a smart pointer is equal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The smart pointer.</param>
  /// <param name="rhs">The raw pointer.</param>
  ///
  /// <returns>True if the pointers are equal.</returns>
  template<typename U>
  friend bool operator==(const OMPtr<T>& lhs, const U* rhs)
  {
    return lhs.m_object == rhs;
  }

  /// <summary>Check if a smart pointer is equal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The raw pointer.</param>
  /// <param name="rhs">The smart pointer.</param>
  ///
  /// <returns>True if the pointers are equal.</returns>
  template<typename U>
  friend bool operator==(const U* lhs, const OMPtr<T>& rhs)
  {
    return lhs == rhs.m_object;
  }

  /// <summary>Check if a smart pointer is unequal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The smart pointer.</param>
  /// <param name="rhs">The raw pointer.</param>
  ///
  /// <returns>True if the pointers are unequal.</returns>
  template<typename U>
  friend bool operator!=(const OMPtr<T>& lhs, const U* rhs)
  {
    return lhs.m_object != rhs;
  }

  /// <summary>Check if a smart pointer is unequal to a raw pointer.</summary>
  ///
  /// <param name="lhs">The raw pointer.</param>
  /// <param name="rhs">The smart pointer.</param>
  ///
  /// <returns>True if the pointers are unequal.</returns>
  template<typename U>
  friend bool operator!=(const U* lhs, const OMPtr<T>& rhs)
  {
    return lhs != rhs.m_object;
  }

  /// <summary>Check if this smart pointer is equal to another smart pointer.</summary>
  ///
  /// <param name="rhs">The other smart pointer.</param>
  ///
  /// <returns>True if the pointers are equal.</returns>
  template<typename U> bool operator==(const OMPtr<U>& rhs) const;

  /// <summary>Check if this smart pointer is unequal to another smart pointer.</summary>
  ///
  /// <param name="rhs">The other smart pointer.</param>
  ///
  /// <returns>True if the pointers are unequal.</returns>
  template<typename U> bool operator!=(const OMPtr<U>& rhs) const;

  template<typename U>
  friend class OMPtr;

private:
  T* m_object;
};

} // end ns sdk2
} // end ns om

#include "om/sdk2/object.inl"

#endif //OM_SDK2_OBJECT_H
