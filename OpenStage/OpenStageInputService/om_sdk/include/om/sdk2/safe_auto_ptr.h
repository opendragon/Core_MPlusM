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
#ifndef OM_SDK2_SAFE_AUTO_PTR_H
#define OM_SDK2_SAFE_AUTO_PTR_H

namespace om
{
namespace sdk2
{

/// <summary>Safe automatic pointer class that provides an alternative to
///   std::auto_ptr without destructive copy.  This class is provided to avoid
///   requiring C++11 or a library such as Boost.
/// </summary>
template<typename T>
class SafeAutoPtr
{
private:
  // Disallow copy construction and assignment.
  SafeAutoPtr(const SafeAutoPtr& rhs);
  const SafeAutoPtr& operator=(const SafeAutoPtr& rhs);

public:
  SafeAutoPtr();
  ~SafeAutoPtr();

  /// <summary>Construct a smart pointer from a raw pointer.</summary>
  ///
  /// <param name="p">A raw pointer.</param>
  SafeAutoPtr(T* p);

  /// <summary>Set this smart pointer to a given raw pointer.</summary>
  ///
  /// <remarks><para>The assignment takes ownership of the pointer.  Do not
  ///   assign a pointer to multiple SafeAutoPtr&lt;T&gt; or else it will be
  ///   deleted more than once, causing a crash.
  /// </para></remarks>
  ///
  /// <param name="p">A raw pointer.</param>
  ///
  /// <returns>A reference to this object.</returns>
  SafeAutoPtr& operator=(T* p);

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

  /// <summary>Query if the pointer is valid (non-null).</summary>
  ///
  /// <returns>True if it is valid or false if it is not.</returns>
  bool IsValid() const;

  /// <summary>Gets the pointer.</summary>
  ///
  /// <returns>The pointer.</returns>
  T* GetPtr() const;

private:
  T* m_p;
};

} // end ns sdk2
} // end ns om

#include "om/sdk2/safe_auto_ptr.inl"

#endif //OM_SDK2_SAFE_AUTO_PTR_H
