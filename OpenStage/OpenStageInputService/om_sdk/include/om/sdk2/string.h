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
#ifndef OM_SDK2_STRING_H
#define OM_SDK2_STRING_H

#include "om/sdk2/object.h"
#include "om/sdk2/declspec.h"

namespace om
{
namespace sdk2
{

/// <summary>A string stored by the SDK.</summary>
class IString : public IOMObject
{
public:
  /// <summary>Get the length of the string.</summary>
  ///
  /// <returns>The length of the string.</returns>
  virtual size_t OMCALL GetSize() const = 0;

  /// <summary>Query if this string is empty.</summary>
  ///
  /// <returns>True if it is empty or false if not.</returns>
  virtual bool OMCALL IsEmpty() const = 0;

  /// <summary>Get the string data.</summary>
  ///
  /// <returns>A pointer to a null-terminated string.</returns>
  virtual const char* OMCALL GetData() const = 0;

  /// <summary>Set the string data.</summary>
  ///
  /// <param name="data">A pointer to a null-terminated string.</param>
  ///
  /// <remarks><para>The string passed to this function will be copied.  The
  ///   object will not take ownership of the string.
  /// </para></remarks>
  virtual void OMCALL SetData(const char* data) = 0;

  /// <summary>Test if this string is considered equal to another.</summary>
  ///
  /// <param name="other">The other string to compare to this object.</param>
  ///
  /// <returns>True if the strings are equal or false otherwise.</returns>
  virtual bool OMCALL Equals(const char* other) const = 0;

  /// <summary>Get the string data.</summary>
  ///
  /// <returns>A pointer to a null-terminated string.</returns>
  operator const char*() const;
};

/// <summary>Smart pointer reference to a string interface.</summary>
typedef OMPtr<IString> StringPtr;

/// <summary>Constant smart pointer reference to a string interface.</summary>
typedef OMPtr<const IString> StringConstPtr;

/// <summary>Creates a string.</summary>
///
/// <param name="data">Pointer to a null-terminated string that will be used as
///   the initial data in the string object, or null to use the empty string.
/// </param>
///
/// <remarks><para>The string passed to this function will be copied.  The
///   created object will not take ownership of the string.
/// </para></remarks>
///
/// <returns>A newly created string object.</returns>
StringPtr CreateString(const char* data = NULL);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IString* OMCALL _CreateString(const char* data);
#endif

inline IString::operator const char*() const
{
  return GetData();
}

inline StringPtr CreateString(const char* data)
{
  return StringPtr(_CreateString(data));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_STRING_H
