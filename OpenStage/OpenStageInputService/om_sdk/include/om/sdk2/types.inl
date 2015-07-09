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
namespace om
{
namespace sdk2
{

//------------------------------------------------------------------------------
// Vector2

inline bool Vector2::operator==(const Vector2& rhs) const
{
  return x == rhs.x && y == rhs.y;
}

inline bool Vector2::operator!=(const Vector2& rhs) const
{
  return x != rhs.x || y != rhs.y;
}

inline const Vector2& Vector2Zero()
{
  static const Vector2 s_vector2Zero = {0.0f, 0.0f};
  return s_vector2Zero;
}

//------------------------------------------------------------------------------
// Vector3

inline bool Vector3::operator==(const Vector3& rhs) const
{
  return x == rhs.x && y == rhs.y && z == rhs.z;
}

inline bool Vector3::operator!=(const Vector3& rhs) const
{
  return x != rhs.x || y != rhs.y || z != rhs.z;
}

inline const Vector3& Vector3Zero()
{
  static const Vector3 s_vector3Zero = {0.0f, 0.0f, 0.0f};
  return s_vector3Zero;
}

//------------------------------------------------------------------------------
// Matrix33

inline bool Matrix33::operator==(const Matrix33& rhs) const
{
  return memcmp(m, rhs.m, sizeof(Matrix33)) == 0;
}

inline bool Matrix33::operator!=(const Matrix33& rhs) const
{
  return !(*this == rhs);
}

inline const Matrix33& Matrix33Zero()
{
  static Matrix33 s_Matrix33Zero = {{{0.0f, 0.0f, 0.0f},
                                     {0.0f, 0.0f, 0.0f},
                                     {0.0f, 0.0f, 0.0f}}};
  return s_Matrix33Zero;
}

inline const Matrix33& Matrix33Identity()
{
  static Matrix33 s_Matrix33Identity = {{{1.0f, 0.0f, 0.0f},
                                         {0.0f, 1.0f, 0.0f},
                                         {0.0f, 0.0f, 1.0f}}};
  return s_Matrix33Identity;
}

//------------------------------------------------------------------------------
// Matrix44

inline bool Matrix44::operator==(const Matrix44& rhs) const
{
  return memcmp(m, rhs.m, sizeof(Matrix44)) == 0;
}

inline bool Matrix44::operator!=(const Matrix44& rhs) const
{
  return !(*this == rhs);
}

inline const Matrix44& Matrix44Zero()
{
  static Matrix44 s_Matrix44Zero = {{{0.0f, 0.0f, 0.0f, 0.0f},
                                     {0.0f, 0.0f, 0.0f, 0.0f},
                                     {0.0f, 0.0f, 0.0f, 0.0f},
                                     {0.0f, 0.0f, 0.0f, 0.0f}}};
  return s_Matrix44Zero;
}

inline const Matrix44& Matrix44Identity()
{
  static Matrix44 s_Matrix44Identity = {{{1.0f, 0.0f, 0.0f, 0.0f},
                                         {0.0f, 1.0f, 0.0f, 0.0f},
                                         {0.0f, 0.0f, 1.0f, 0.0f},
                                         {0.0f, 0.0f, 0.0f, 1.0f}}};
  return s_Matrix44Identity;
}

//------------------------------------------------------------------------------
// Version

inline Version::Version()
: major(0),
  minor(0),
  revision(0),
  changelist(0)
{}

inline Version::Version(const Version& other)
: major(other.major),
  minor(other.minor),
  revision(other.revision),
  changelist(other.changelist)
{}

inline Version::Version(int major_, int minor_, int revision_, int changelist_)
: major(major_),
  minor(minor_),
  revision(revision_),
  changelist(changelist_)
{}

inline Version& Version::operator=(const Version& other)
{
  major = other.major;
  minor = other.minor;
  revision = other.revision;
  changelist = other.changelist;
  return *this;
}

inline bool Version::operator==(const Version& other) const
{
  return major == other.major &&
         minor == other.minor &&
         revision == other.revision &&
         changelist == other.changelist;
}

inline bool Version::operator!=(const Version& other) const
{
  return !(*this == other);
}

inline bool Version::operator<(const Version& other) const
{
  if (major == other.major)
  {
    if (minor == other.minor)
    {
      if (revision == other.revision)
      {
        return changelist < other.changelist;
      }
      return revision < other.revision;
    }
    return minor < other.minor;
  }
  return major < other.major;
}

inline bool Version::operator<=(const Version& other) const
{
  if (major == other.major)
  {
    if (minor == other.minor)
    {
      if (revision == other.revision)
      {
        return changelist <= other.changelist;
      }
      return revision < other.revision;
    }
    return minor < other.minor;
  }
  return major < other.major;
}

inline bool Version::operator>(const Version& other) const
{
  return other < *this;
}

inline bool Version::operator>=(const Version& other) const
{
  return other <= *this;
}

inline std::ostream& operator<<(std::ostream& os, const Version& version)
{
  os << version.major << "." << version.minor << "." << version.revision << "."
     << version.changelist;
  return os;
}

//------------------------------------------------------------------------------
// Date

inline Date::Date()
: year(0),
  month(0),
  day(0)
{}

inline Date::Date(const Date& other)
: year(other.year),
  month(other.month),
  day(other.day)
{}

inline Date::Date(int year_, int month_, int day_)
: year(year_),
  month(month_),
  day(day_)
{}

inline Date& Date::operator=(const Date& other)
{
  year = other.year;
  month = other.month;
  day = other.day;
  return *this;
}

inline bool Date::operator==(const Date& other) const
{
  return year == other.year && month == other.month && day == other.day;
}

inline bool Date::operator!=(const Date& other) const
{
  return !(*this == other);
}

inline bool Date::operator<(const Date& other) const
{
  if (year == other.year)
  {
    if (month == other.month)
    {
      return day < other.day;
    }
    return month < other.month;
  }
  return year < other.year;
}

inline bool Date::operator<=(const Date& other) const
{
  if (year == other.year)
  {
    if (month == other.month)
    {
      return day <= other.day;
    }
    return month < other.month;
  }
  return year < other.year;
}

inline bool Date::operator>(const Date& other) const
{
  return other < *this;
}

inline bool Date::operator>=(const Date& other) const
{
  return other <= *this;
}

inline std::ostream& operator<<(std::ostream& os, const Date& date)
{
  os << date.month << "/" << date.day << "/" << date.year;
  return os;
}

//------------------------------------------------------------------------------
// BytesPerPixel

inline int BytesPerPixel(PixelFormat format) 
{
  switch (format)
  {
    case PixelFormat_R8uG8uB8uA8u:
      return 4*sizeof(unsigned char);
    case PixelFormat_R32fG32fB32fA32f:
      return 4*sizeof(float);
    case PixelFormat_R8u:
      return 1*sizeof(unsigned char);
    case PixelFormat_R32f:
      return 1*sizeof(float);
    case PixelFormat_R16u:
    case PixelFormat_R8uG8u:
      return 2*sizeof(unsigned char);
    case PixelFormat_R8uG8uB8u:
      return 3*sizeof(unsigned char);
    default:
      return 0;
  }
}

//------------------------------------------------------------------------------
// CalibrationStatus

inline void CalibrationStatus::Clear()
{
  numWandsDetected.Clear();
  missingCameras.Clear();
}

//------------------------------------------------------------------------------
// CalibrationResult

inline CalibrationResult::CalibrationResult()
: reprojErrorOptimizedMean(-1.0f),
  reprojErrorOptimizedStdDev(-1.0f),
  reprojErrorTestMean(-1.0f),
  reprojErrorTestStdDev(-1.0f)
{}

//------------------------------------------------------------------------------
// ActorSlot

inline ActorSlot::ActorSlot()
: profileId(),
  defaultSkeleton()
{}

//------------------------------------------------------------------------------
// CameraPropertyValue

inline CameraPropertyValue::CameraPropertyValue()
: valueA(0.0f),
  valueB(0.0f)
{}

inline CameraPropertyValue::CameraPropertyValue(float valueA_)
: valueA(valueA_),
  valueB(0.0f)
{}

inline CameraPropertyValue::CameraPropertyValue(float valueA_, float valueB_)
: valueA(valueA_),
  valueB(valueB_)
{}

inline CameraPropertyValue::CameraPropertyValue(
  const CameraPropertyValue& propVal)
: valueA(propVal.valueA),
  valueB(propVal.valueB)
{}

inline CameraPropertyValue& CameraPropertyValue::operator=(
  const CameraPropertyValue& propVal)
{
  valueA = propVal.valueA;
  valueB = propVal.valueB;
  return *this;
}

inline bool CameraPropertyValue::operator==(
  const CameraPropertyValue& propVal) const
{
  return valueA == propVal.valueA && valueB == propVal.valueB;
}

//------------------------------------------------------------------------------
// Notification

inline void Notification::Clear()
{
  summary.Clear();
  detail.Clear();
}

//------------------------------------------------------------------------------
// VideoRecordStatus

inline void VideoRecordStatus::Clear()
{
  framesRecorded = 0;
  aborted = false;
}

} // end ns sdk2
} // end ns om
