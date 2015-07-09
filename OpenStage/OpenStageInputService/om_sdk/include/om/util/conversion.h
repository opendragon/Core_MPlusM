/*
 * =============================================================================
 *  Copyright(c) 2011-2012 Organic Motion, Inc. All Rights Reserved.
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
#ifndef OM_UTIL_CONVERSION_H
#define OM_UTIL_CONVERSION_H

namespace om
{
namespace utils
{

/// <summary>Helper class to check whether there is an implicit conversion
/// between two types.</summary>
///
/// <remarks><para>This class is based on the example given on pages 35-37 of
/// Modern C++ Design by Andrei Alexandrescu.</para></remarks>
///
/// <param name="T">The type to convert from.</param>
/// <param name="U">The type to convert to.</param>
template<typename T, typename U>
class Conversion
{
private:
  struct Yes
  {
    char data[1];
  };
  struct No
  {
    char data[2];
  };

  static Yes CheckForConversion(const U&);
  static No CheckForConversion(...);
  static const T& GetT();

public:
  /// <summary></summary>
  enum
  {
    /// <summary>true if the conversion exists, false otherwise.</summary>
    Exists
#ifndef DOXYGEN_INVOKED
      = sizeof(CheckForConversion(GetT())) == sizeof(Yes)
#endif
  };
};

} // end ns utils
} // end ns om

#endif //OM_UTIL_CONVERSION_H
