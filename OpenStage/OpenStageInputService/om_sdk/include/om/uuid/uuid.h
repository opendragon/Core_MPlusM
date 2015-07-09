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
#ifndef OM_UUID_UUID_H
#define OM_UUID_UUID_H

#include <string.h>

#include <ostream>
#include <iomanip>

namespace om
{
namespace uuid
{

/// <summary>Universally unique identifier.</summary>
struct Uuid
{
  /// <summary>The data.</summary>
  unsigned char data[16];
};

/// <summary>Checks if two UUIDs are equal.</summary>
///
/// <param name="uuid1">The first UUID.</param>
/// <param name="uuid2">The second UUID.</param>
///
/// <returns>true if the parameters are considered equivalent.</returns>
inline bool operator==(const Uuid& uuid1, const Uuid& uuid2)
{
  return 0 == memcmp(&uuid1, &uuid2, sizeof(uuid1));
}

/// <summary>Checks if two UUIDs are unequal.</summary>
///
/// <param name="uuid1">The first UUID.</param>
/// <param name="uuid2">The second UUID.</param>
///
/// <returns>true if the parameters are not considered equivalent.</returns>
inline bool operator!=(const Uuid& uuid1, const Uuid& uuid2)
{
  return 0 != memcmp(&uuid1, &uuid2, sizeof(uuid1));
}

/// <summary>Checks if a UUID is less than another UUID.</summary>
///
/// <param name="uuid1">The first UUID.</param>
/// <param name="uuid2">The second UUID.</param>
///
/// <returns>true if the first parameter is less than the second.</returns>
inline bool operator<(const Uuid& uuid1, const Uuid& uuid2)
{
  return memcmp(&uuid1, &uuid2, sizeof(uuid1)) < 0;
}

/// <summary>Checks if a UUID is less than or equal to another UUID.</summary>
///
/// <param name="uuid1">The first UUID.</param>
/// <param name="uuid2">The second UUID.</param>
///
/// <returns>true if the first parameter is less than or equal to the second.</returns>
inline bool operator<=(const Uuid& uuid1, const Uuid& uuid2)
{
  return memcmp(&uuid1, &uuid2, sizeof(uuid1)) <= 0;
}

/// <summary>Checks if a UUID is greater than another UUID.</summary>
///
/// <param name="uuid1">The first UUID.</param>
/// <param name="uuid2">The second UUID.</param>
///
/// <returns>true if the first parameter is greater than to the second.</returns>
inline bool operator>(const Uuid& uuid1, const Uuid& uuid2)
{
  return memcmp(&uuid1, &uuid2, sizeof(uuid1)) > 0;
}

/// <summary>Checks if a UUID is greater than or equal to another UUID.</summary>
///
/// <param name="uuid1">The first UUID.</param>
/// <param name="uuid2">The second UUID.</param>
///
/// <returns>true if the first parameter is greater than or equal to the second.</returns>
inline bool operator>=(const Uuid& uuid1, const Uuid& uuid2)
{
  return memcmp(&uuid1, &uuid2, sizeof(uuid2)) >= 0;
}

/// <summary>Print a UUID to an output stream.</summary>
///
/// <param name="strm">The output stream.</param>
/// <param name="uuid">The UUID.</param>
///
/// <returns>The output stream.</returns>
inline std::ostream& operator<<(std::ostream& strm, const Uuid& uuid)
{
  std::ios_base::fmtflags oldFlags = strm.flags();
  char oldFill = strm.fill();
  std::hex(strm);
  strm.fill('0');
  for (int i = 0; i < sizeof(uuid.data) / sizeof(uuid.data[0]); ++i)
  {
    if (4 == i || 6 == i || 8 == i || 10 == i)
    {
      strm << "-";
    }
    strm << std::setw(2) << static_cast<int>(uuid.data[i]);
  }
  strm.flags(oldFlags);
  strm.fill(oldFill);
  return strm;
}

} // end ns uuid
} // end ns om

#endif //OM_UUID_UUID_H
