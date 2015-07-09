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
#ifndef OM_SDK2_SAFE_REF_H
#define OM_SDK2_SAFE_REF_H

namespace om
{
namespace sdk2
{

/// <summary>Object encapsulating a reference to another object.  The purpose
///   of this class is to avoid errors involving references to references when
///   building with certain compilers, such as Visual C++ 2005.
/// </summary>
template<typename T>
class SafeRef
{
public:
  /// <summary>Construct a safe reference to an object.</summary>
  ///
  /// <param name="x">The object.</param>
  SafeRef(T& x);

  /// <summary>Assign to the object referenced by this safe reference.</summary>
  ///
  /// <param name="x">The object to assign to the safe reference.</param>
  ///
  /// <returns>A reference to the safe reference.</returns>
  SafeRef& operator=(const T& x);

  /// <summary>Convert the safe reference to an ordinary reference.</summary>
  ///
  /// <returns>A reference to the object referenced by the safe
  ///   reference.
  /// </returns>
  /**@{*/
  operator T&();
  operator const T&() const;
  /**@}*/

private:
  T& m_x;
};

} // end ns sdk2
} // end ns om

#include "om/sdk2/safe_ref.inl"

#endif
