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
#ifndef OM_SDK2_TYPES_H
#define OM_SDK2_TYPES_H

#include "om/sdk2/string.h"
#include "om/sdk2/list.h"
#include "om/sdk2/tree.h"
#include "om/uuid/uuid.h"
#include <ostream>

namespace om
{
namespace sdk2
{

// Bring the UUID data type into the SDK namespace.
using om::uuid::Uuid;

// Forward declaration.
class IImageArray;

/// <summary>A 2D vector.</summary>
struct Vector2
{
  /// <summary>Check if two vectors are equal.</summary>
  ///
  /// <param name="rhs">Another vector.</param>
  ///
  /// <returns>True if the vectors are equal or false otherwise.</returns>
  bool operator==(const Vector2& rhs) const;

  /// <summary>Check if two vectors are unequal.</summary>
  ///
  /// <param name="rhs">Another vector.</param>
  ///
  /// <returns>True if the vectors are unequal or false otherwise.</returns>
  bool operator!=(const Vector2& rhs) const;

  /// <summary>The <i>x</i>-coordinate.</summary>
  float x;
  /// <summary>The <i>y</i>-coordinate.</summary>
  float y;
};

/// <summary>Obtain the 2D zero vector.</summary>
///
/// <returns>The 2D zero vector.</returns>
const Vector2& Vector2Zero();

/// <summary>A 3D vector.</summary>
struct Vector3
{
  /// <summary>Check if two vectors are equal.</summary>
  ///
  /// <param name="rhs">Another vector.</param>
  ///
  /// <returns>True if the vectors are equal or false otherwise.</returns>
  bool operator==(const Vector3& rhs) const;

  /// <summary>Check if two vectors are unequal.</summary>
  ///
  /// <param name="rhs">Another vector.</param>
  ///
  /// <returns>True if the vectors are unequal or false otherwise.</returns>
  bool operator!=(const Vector3& rhs) const;

  /// <summary>The <i>x</i>-coordinate.</summary>
  float x;
  /// <summary>The <i>y</i>-coordinate.</summary>
  float y;
  /// <summary>The <i>z</i>-coordinate.</summary>
  float z;
};

/// <summary>Obtain the 3D zero vector.</summary>
///
/// <returns>The 3D zero vector.</returns>
const Vector3& Vector3Zero();

/// <summary>A 3 &times; 3 matrix.</summary>
struct Matrix33
{
  /// <summary>Check if two matrices are equal.</summary>
  ///
  /// <param name="rhs">Another matrix.</param>
  ///
  /// <returns>True if the matrices are equal or false otherwise.</returns>
  bool operator==(const Matrix33& rhs) const;

  /// <summary>Check if two matrices are unequal.</summary>
  ///
  /// <param name="rhs">Another matrix.</param>
  ///
  /// <returns>True if the matrices are unequal or false otherwise.</returns>
  bool operator!=(const Matrix33& rhs) const;

  /// <summary>The row-major matrix data.</summary>
  float m[3][3];
};

/// <summary>Obtain the 3 &times; 3 zero matrix.</summary>
///
/// <returns>The 3 &times; 3 zero matrix.</returns>
const Matrix33& Matrix33Zero();

/// <summary>Obtain the 3 &times; 3 identity matrix.</summary>
///
/// <returns>The 3 &times; 3 identity matrix.</returns>
const Matrix33& Matrix33Identity();

/// <summary>A 4 & times; 4 matrix.</summary>
struct Matrix44
{
  /// <summary>Check if two matrices are equal.</summary>
  ///
  /// <param name="rhs">Another matrix.</param>
  ///
  /// <returns>True if the matrices are equal or false otherwise.</returns>
  bool operator==(const Matrix44& rhs) const;

  /// <summary>Check if two matrices are unequal.</summary>
  ///
  /// <param name="rhs">Another matrix.</param>
  ///
  /// <returns>True if the matrices are unequal or false otherwise.</returns>
  bool operator!=(const Matrix44& rhs) const;
  
  /// <summary>The row-major matrix data.</summary>
  float m[4][4];
};

/// <summary>Obtain the 4 &times; 4 zero matrix.</summary>
///
/// <returns>The 4 &times; 4 zero matrix.</returns>
const Matrix44& Matrix44Zero();

/// <summary>Obtain the 4 &times; 4 identity matrix.</summary>
///
/// <returns>The 4 &times; 4 identity matrix.</returns>
const Matrix44& Matrix44Identity();

/// <summary>A software version number.</summary>
struct Version
{
  Version();

  /// <summary>Initialize this version number from another version number.</summary>
  ///
  /// <param name="other">Another version number.</param>
  Version(const Version& other);

  /// <summary>Initialize this version number to a specific value.</summary>
  ///
  /// <param name="major_">The major version number.</param>
  /// <param name="minor_">The minor version number.</param>
  /// <param name="revision_"> The revision.</param>
  /// <param name="changelist_">The changelist number.</param>
  Version(int major_, int minor_, int revision_, int changelist_);

  /// <summary>Set this version number to be equal to another.</summary>
  ///
  /// <param name="other">Another version number.</param>
  ///
  /// <returns>A reference to this object.</returns>
  Version& operator=(const Version& other);

  /// <summary>Check if two version numbers are equal.</summary>
  ///
  /// <param name="other">Another version number.</param>
  ///
  /// <returns>True if the version numbers are equal or false otherwise.</returns>
  bool operator==(const Version& other) const;

  /// <summary>Check if two version numbers are unequal.</summary>
  ///
  /// <param name="other">Another version number.</param>
  ///
  /// <returns>True if the version numbers are unequal or false otherwise.</returns>
  bool operator!=(const Version& other) const;

  /// <summary>Check if this version number is less than another.</summary>
  ///
  /// <param name="other">Another version number.</param>
  ///
  /// <returns>True if the first version number is less than the second.</returns>
  bool operator<(const Version& other) const;

  /// <summary>Check if this version number is less than or equal to another.</summary>
  ///
  /// <param name="other">Another version number.</param>
  ///
  /// <returns>True if the first version number is less than or equal to the
  ///   second.
  /// </returns>
  bool operator<=(const Version& other) const;

  /// <summary>Check if this version number is greater than another.</summary>
  ///
  /// <param name="other">Another version number.</param>
  ///
  /// <returns>True if the first version is greater than the second.</returns>
  bool operator>(const Version& other) const;

  /// <summary>Check if this version number is greater than or equal to another.</summary>
  ///
  /// <param name="other">Another version number.</param>
  ///
  /// <returns>True if the first version is greater than or equal to the second.</returns>
  bool operator>=(const Version& other) const;

  /// <summary>The major version number.</summary>
  int major;
  /// <summary>The minor version number.</summary>
  int minor;
  /// <summary>The revision.</summary>
  int revision;
  /// <summary>The changelist number.</summary>
  int changelist;
};

/// <summary>Print a version number to an output stream.</summary>
///
/// <param name="os">An output stream.</param>
/// <param name="version">A version number.</param>
///
/// <returns>The output stream.</returns>
std::ostream& operator<<(std::ostream& os, const Version& version);

/// <summary>A date.</summary>
struct Date
{
  Date();

  /// <summary>Initialize this date from another date.</summary>
  ///
  /// <param name="other">Another date.</param>
  Date(const Date& other);

  /// <summary>Initialize this date to a specific value.</summary>
  ///
  /// <param name="year_">The year.</param>
  /// <param name="month_">The month.</param>
  /// <param name="day_">The day.</param>
  Date(int year_, int month_, int day_);

  /// <summary>Set this date to be equal to another.</summary>
  ///
  /// <param name="other">Another date.</param>
  ///
  /// <returns>A reference to this object.</returns>
  Date& operator=(const Date& other);

  /// <summary>Check if two dates are equal.</summary>
  ///
  /// <param name="other">Another date.</param>
  ///
  /// <returns>True if the two dates are equal or false otherwise.</returns>
  bool operator==(const Date& other) const;

  /// <summary>Check if two dates are unequal.</summary>
  ///
  /// <param name="other">Another date.</param>
  ///
  /// <returns>True if the two dates are unequal or false otherwise.</returns>
  bool operator!=(const Date& other) const;

  /// <summary>Check if this date is less than another.</summary>
  ///
  /// <param name="other">Another date.</param>
  ///
  /// <returns>True if the first date is less than the second.</returns>
  bool operator<(const Date& other) const;

  /// <summary>Check if this date is less than or equal to another.</summary>
  ///
  /// <param name="other">Another date.</param>
  ///
  /// <returns>True if the first date is less than or equal to the
  ///   second.
  /// </returns>
  bool operator<=(const Date& other) const;

  /// <summary>Check if this date is greater than another.</summary>
  ///
  /// <param name="other">Another date.</param>
  ///
  /// <returns>True if the first date is greater than the second.</returns>
  bool operator>(const Date& other) const;

  /// <summary>Check if this date is greater than or equal to another.</summary>
  ///
  /// <param name="other">Another date.</param>
  ///
  /// <returns>True if the first date is greater than or equal to the
  ///   second.
  /// </returns>
  bool operator>=(const Date& other) const;

  /// <summary>The year.</summary>
  int year;
  /// <summary>The month.</summary>
  int month;
  /// <summary>The day.</summary>
  int day;
};

/// <summary>Print a date to an output stream.</summary>
///
/// <param name="os">An output stream.</param>
/// <param name="date">A date.</param>
///
/// <returns>The output stream.</returns>
std::ostream& operator<<(std::ostream& os, const Date& date);

/// <summary>Timing information about a frame streamed from the
///   server.
/// </summary>
struct TimeInfo
{
  /// <summary>The frame identifier reported from the sensor.</summary>
  int frameId;
  /// <summary>The timestamp reported from the sensor, the number of seconds
  ///   elapsed since the sensor was started.
  /// </summary>
  double timestamp;
  /// <summary>The timestamp identifying when the frame was processed.  If the
  ///   frame is from a video, this timestamp will give the time at which the
  ///   frame was read from the video file on the server.  This timestamp is a
  ///   64-bit value representing the number of 100-nanosecond intervals since
  ///   January 1, 1601 (UTC).  This is the same value used in the Win32
  ///   FILETIME data structure.
  /// </summary>
  unsigned long long timestampProcessed;
  /// <summary>Event marker signal reported from the system.  This value will
  ///   be 0 if no event coincided or 1 if an event coincided.
  /// </summary>
  int eventMarker;
  /// <summary>The frame number of the frame in the video if the frame was
  ///   streamed from a video.  This value will be negative if the frame is
  ///   not from a video.
  /// </summary>
  int frameNumber;
};

/// <summary>A hierarchical segment.</summary>
struct Segment
{
  /// <summary>The transform.</summary>
  Matrix44 transform; 
  /// <summary>The length.</summary>
  float length;
};

/// <summary>Actor data.</summary>
struct ActorData
{
  /// <summary>The actor identifier.</summary>
  StringPtr id;
  /// <summary>Timing information for the frame.</summary>
  TimeInfo timeInfo;
  /// <summary>The skeleton.</summary>
  SkeletonPtr skeleton;
};

/// <summary>Values that represent pixel formats.</summary>
enum PixelFormat
{
  /// <summary>Four 32-bit floating-point components.</summary>
  PixelFormat_R32fG32fB32fA32f = 0,
  /// <summary>One 32-bit floating-point component.</summary>
  PixelFormat_R32f,
  /// <summary>Four 8-bit integer components.</summary>
  PixelFormat_R8uG8uB8uA8u,
  /// <summary>Three 8-bit integer components.</summary>
  PixelFormat_R8uG8uB8u,
  /// <summary>One 8-bit integer component.</summary>
  PixelFormat_R8u,
  /// <summary>One 16-bit integer component.</summary>
  PixelFormat_R16u,
  /// <summary>Two 8-bit integer components.</summary>
  ///
  /// <remarks><para>This pixel format is used for YUV422, where the first
  ///   component specifies the Y value and the second component alternates
  ///   between U and V.
  /// </para></remarks>
  PixelFormat_R8uG8u,
  /// <summary>An unknown pixel format.</summary>
  PixelFormat_Unknown,
};

/// <summary>Get the number of bytes per pixel for the given pixel format.</summary>
///
/// <param name="format">A pixel format.</param>
///
/// <returns>The number of bytes per pixel.</returns>
int BytesPerPixel(PixelFormat format);

/// <summary>Image array header.</summary>
struct ImageArrayHeader
{
  /// <summary>The pixel format.</summary>
  PixelFormat format;
  /// <summary>The width.</summary>
  int width;
  /// <summary>The height.</summary>
  int height;
  /// <summary>The array depth (number of images).</summary>
  int depth;
  /// <summary>Timing information for the frame.</summary>
  TimeInfo timeInfo;
};

/// <summary>Possible image formats for a compressed image array.</summary>
enum ImageFormat
{
  /// <summary>JPEG images.</summary>
  ImageFormat_JPEG
};

/// <summary>Compressed image array header.</summary>
struct CompressedImageArrayHeader
{
  /// <summary>The image format.</summary>
  ImageFormat format;
  /// <summary>The array depth (number of images).</summary>
  int depth;
  /// <summary>Timing information for the frame.</summary>
  TimeInfo timeInfo;
};

/// <summary>Values that represent room calibration state.</summary>
enum CalibrationState
{
  /// <summary>Calibration is not currently in progress.</summary>
  CalibrationState_Off,
  /// <summary>Calibration has been initialized but not yet started.</summary>
  CalibrationState_Initialized,
  /// <summary>The server is acquiring points from the wand.</summary>
  CalibrationState_FindingWand,
  /// <summary>The server has acquired a sufficient number of points from the
  ///   wand and is ready to move on to triangle acquisition.</summary>
  CalibrationState_WandFound,
  /// <summary>The server is acquiring the triangle.</summary>
  CalibrationState_FindingTriangle,
  /// <summary>The server has found the triangle, and calibration is complete.</summary>
  CalibrationState_TriangleFound,
};

/// <summary>Room calibration status.</summary>
struct CalibrationStatus
{
  /// <summary>Clear this object to its blank/initial state.</summary>
  void Clear();

  /// <summary>The current calibration state.</summary>
  CalibrationState state;
  /// <summary>A list of integers, one for each camera, giving the number of
  ///   wand points acquired for that camera.
  /// </summary>
  IntListPtr numWandsDetected;
  /// <summary>A list of integers containing the camera indices of those cameras
  ///   for which a sufficient number of points could not be acquired using the
  ///   wand.
  /// </summary>
  IntListPtr missingCameras;
};

/// <summary>Room calibration result.</summary>
struct CalibrationResult
{
  CalibrationResult();

  /// <summary>The optimized reprojection error mean.</summary>
  float reprojErrorOptimizedMean;
  /// <summary>The optimized reprojection error standard deviation.</summary>
  float reprojErrorOptimizedStdDev;
  /// <summary>The test set reprojection error mean.</summary>
  float reprojErrorTestMean;
  /// <summary>The test set reprojection error standard deviation.</summary>
  float reprojErrorTestStdDev;
};

/// <summary>Information on an actor slot.</summary>
///
/// <remarks><para>Actor slots are placeholders for actor instances.  Every
///   actor currently tracked in OpenStage occupies a slot.
/// </summary>
struct ActorSlot
{
  ActorSlot();

  /// <summary>Identifier of the profile used for the actor acquired in this
  ///   slot.
  /// </summary>
  StringPtr profileId;
  /// <summary>Default skeleton.</summary>
  SkeletonPtr defaultSkeleton;
};

/// <summary>Actor joint.</summary>
struct Joint
{
  /// <summary>The transform.</summary>
  Matrix44 transform;
};

/// <summary>Camera distortion coefficients.</summary>
struct Distortion
{
  /// <summary>The coefficients.</summary>
  double coefficients[5];
};

/// <summary>Camera frustum.</summary>
struct Frustum
{
  /// <summary>The camera <i>x</i>-axis (right).</summary>
  Vector3 axisX;
  /// <summary>The camera <i>y</i>-axis (up).</summary>
  Vector3 axisY;
  /// <summary>The camera <i>z</i>-axis (principal axis).</summary>
  Vector3 axisZ;
  /// <summary>The camera center.</summary>
  Vector3 center;
  /// <summary>The near <i>z</i> clip distance.</summary>
  float nearZ;
  /// <summary>The far <i>z</i> clip distance.</summary>
  float farZ;
  /// <summary>The aspect ratio.</summary>
  float aspectRatio;
  /// <summary>The horizontal field-of-view in radians.</summary>
  float fovHRad;
  /// <summary>The vertical field-of-view in radians.</summary>
  float fovVRad;
};

/// <summary>Axis aligned box.</summary>
struct AxisAlignedBox
{
  /// <summary>The center.</summary>
  Vector3 center;
  /// <summary>The extents.</summary>
  Vector3 extents;
};

/// <summary>Flags to specify one or more axes.</summary>
enum AxisMask
{
  /// <summary>The <i>x</i>-axis.</summary>
  AxisMask_X = 1 << 0,
  /// <summary>The <i>y</i>-axis.</summary>
  AxisMask_Y = 1 << 1,
  /// <summary>The <i>z</i>-axis.</summary>
  AxisMask_Z = 1 << 2
};

/// <summary>Voxel box.</summary>
struct VoxelBox
{
  /// <summary>The axis aligned box.</summary>
  AxisAlignedBox aab;
  /// <summary>The resolution of the voxel volume.</summary>
  Vector3 resolution;
  /// <summary>The bitwise OR of zero or more <see cref="AxisMask" /> flags
  ///   specifying the axes along which slices are placed.
  /// </summary>
  int axisMask;
};

/// <summary>Voxel data.</summary>
struct VoxelData
{
  /// <summary>The voxel box.</summary>
  VoxelBox voxelBox;
  /// <summary>The voxel data image.</summary>
  OMPtr<IImageArray> voxelImage;
  /// <summary>The index of this voxel box.</summary>
  int index;
  /// <summary>The number of voxel boxes being streamed.</summary>
  int count;
};

/// <summary>Values that represent camera properties.</summary>
enum CameraProperty
{
  /// <summary>Shutter.</summary>
  CameraProperty_Shutter = 0,
  /// <summary>Gain.</summary>
  CameraProperty_Gain,
  /// <summary>White balance, with the red value as value A and the blue value
  ///   as value B.
  /// </summary>
  CameraProperty_WhiteBalance,
  /// <summary>Saturation.</summary>
  CameraProperty_Saturation,
  /// <summary>Gamma.</summary>
  CameraProperty_Gamma,
  CameraPropertyCount
};

/// <summary>Information about the camera property.</summary>
struct CameraPropertyInfo
{
  /// <summary>The absolute minimum value.</summary>
  int absMin;
  /// <summary>The absolute maximum value.</summary>
  int absMax;
};

/// <summary>Camera property value.</summary>
///
/// <remarks><para>Camera property values have two components, value A and
///   value B.  For most properties, only value A is meaningful.  For white
///   balance, value A holds the red value and value B holds the blue value.
/// </para></remarks>
struct CameraPropertyValue
{
  CameraPropertyValue();

  /// <summary>Initialize this camera property value.</summary>
  ///
  /// <param name="valueA_">Value A.</param>
  explicit CameraPropertyValue(float valueA_);

  /// <summary>Initialize this camera property value.</summary>
  ///
  /// <param name="valueA_">Value A.</param>
  /// <param name="valueB_">Value B.</param>
  CameraPropertyValue(float valueA_, float valueB_);

  /// <summary>Initialize this camera property value from another camera
  ///   property value.
  /// </summary>
  ///
  /// <param name="propVal">The camera property value to copy.</param>
  CameraPropertyValue(const CameraPropertyValue& propVal);

  /// <summary>Set this camera property value to be equal to another.</summary>
  ///
  /// <param name="propVal">The camera property value to copy.</param>
  ///
  /// <returns>A reference to this object.</returns>
  CameraPropertyValue& operator=(const CameraPropertyValue& propVal);

  /// <summary>Check if this camera property value is equal to another.</summary>
  ///
  /// <param name="propVal">Another camera property value.</param>
  ///
  /// <returns>True if the values are equal.</returns>
  bool operator==(const CameraPropertyValue& propVal) const;

  /// <summary>Property value A.</summary>
  float valueA;
  /// <summary>Property value B.</summary>
  ///
  /// <remarks><para>This is unused for most properties.  For white balance,
  ///   value A holds the red value and value B holds the blue value.
  /// </para></remarks>
  float valueB;
};

/// <summary>Severity levels for system notifications.</summary>
enum NotificationSeverity
{
  /// <summary>An informative notification that does not indicate a problem.</summary>
  NotificationSeverity_Info,
  /// <summary>A notification that could indicate a problem.</summary>
  NotificationSeverity_Warning,
  /// <summary>A notification indicating a definite problem that must be
  ///   addressed before it is possible to track successfully.
  /// </summary>
  NotificationSeverity_Critical,
  /// <summary>The client is currently disconnected from the server.</summary>
  NotificationSeverity_Disconnected,
  NotificationSeverity_Levels
};

/// <summary>Special sentinel value used for
///   <see cref="Notification::when" />.
/// </summary>
enum
{
  /// <summary>Sentinel used to indicate that a notification has been
  ///   dismissed.
  /// </summary>
  Notification_WhenRemoveValue = 0
};

/// <summary>A system notification.</summary>
struct Notification
{
  /// <summary>Clears this object to its blank/initial state.</summary>
  void Clear();

  /// <summary>The identifier (increasing count from first notification).</summary>
  unsigned int id;
  /// <summary>Time at which the notification occurred.</summary>
  time_t when;
  /// <summary>A UUID indicating the type of notification.</summary>
  Uuid type;
  /// <summary>The severity of the notification.</summary>
  NotificationSeverity severity;
  /// <summary>A summary of the issue causing the notification.</summary>
  StringPtr summary;
  /// <summary>A detailed description of the issue causing the notification.</summary>
  StringPtr detail;
};

/// <summary>Values that represent auto exposure state.</summary>
enum AutoExposureState
{
  /// <summary>The last auto exposure operation completed successfully.</summary>
  AutoExposureState_OK = 0,
  /// <summary>The last auto exposure operation failed.</summary>
  AutoExposureState_Fail,
  /// <summary>An auto exposure operation is currently in progress.</summary>
  AutoExposureState_InProgress
};

/// <summary>Values that represent video recording states.</summary>
enum VideoRecordState
{
  /// <summary>The server is not currently recording video.</summary>
  VideoRecordState_Stopped,
  /// <summary>The server is preparing to start recording video.</summary>
  VideoRecordState_Initializing,
  /// <summary>The server is currently recording video.</summary>
  VideoRecordState_Recording,
  /// <summary>The server is finishing saving recorded video to disk.</summary>
  VideoRecordState_Finishing,
  /// <summary>The server experienced a video recording error.</summary>
  VideoRecordState_Error
};

/// <summary>Values that represent video compression types.</summary>
enum VideoCompressionType
{
  /// <summary>No compression.</summary>
  VideoCompressionType_None,
  /// <summary>Keyframe difference compression.</summary>
  VideoCompressionType_KeyframeDifference,
  VideoCompressionTypeCount
};

/// <summary>Video recording parameters.</summary>
struct VideoRecordParameters
{
  /// <summary>Name of the video.</summary>
  StringPtr videoName;
  /// <summary>Type of video compression.</summary>
  VideoCompressionType compressionType;
  /// <summary>The password, or an empty string to indicate no password.</summary>
  StringPtr password;
};

/// <summary>Video recording status data.</summary>
struct VideoRecordStatus
{
  /// <summary>Clear this object to its blank/initial state.</summary>
  void Clear();

  /// <summary>Number of frames recorded so far.</summary>
  int framesRecorded;
  /// <summary>Boolean value indicating if video recording was aborted due to
  ///   low disk space on the server.
  /// </summary>
  bool aborted;
};

/// <summary>Values that represent image sources.</summary>
enum ImageSource
{
  /// <summary>Images from live cameras.</summary>
  ImageSource_Cameras,
  /// <summary>Images from a recorded video.</summary>
  ImageSource_Video
};

/// <summary>Values indicating the state of the last video load
///   operation.
/// </summary>
enum VideoLoadState
{
  /// <summary>A video is currently being loaded.</summary>
  VideoLoadState_Loading,
  /// <summary>A video was successfully loaded.</summary>
  VideoLoadState_Loaded,
  /// <summary>No video is currently loaded.  This value will be returned if the
  ///   last attempt to load a video failed.
  /// </summary>
  VideoLoadState_NotLoaded
};

/// <summary>Values indicating the state of video playback.</summary>
enum VideoPlaybackState
{
  /// <summary>The video is currently playing.</summary>
  VideoPlaybackState_Play,
  /// <summary>The video is currently stopped.</summary>
  VideoPlaybackState_Stop
};

/// <summary>Frame markers stored in video metadata.</summary>
struct FrameMarkers
{
  /// <summary>The frame number at which to start retracking.</summary>
  int startFrame;
  /// <summary>The frame number at which to end retracking.</summary>
  int endFrame;
};

/// <summary>Values indicating device types.</summary>
enum DeviceType
{
  /// <summary>Display adapters.</summary>
  DeviceType_Display,
  /// <summary>Ports.</summary>
  DeviceType_Ports,
  /// <summary>1394 adapters.</summary>
  DeviceType_1394,
  /// <summary>Network adapters</summary>
  DeviceType_Network,

};

/// <summary>Information on a device driver on the server.
struct DriverInfo
{
  /// <summary>The description of driver.</summary>
  StringPtr description;
  /// <summary>The name of the driver's manufacturer.</summary>
  StringPtr manufacturerName;
  /// <summary>The port name (e.g. COM1) if the device is a port, or an empty
  ///   string if the device is not a port. If a GigE camera, this is the IP:PORT of the camera.
  /// </summary>
  StringPtr portName;
  /// <summary>The date on which the driver was published.</summary>
  Date driverDate;
  /// <summary>The driver's version number.</summary>
  Version driverVersion;
};

/// <summary>One frame of inertial sensor data.</summary>
struct InertialSensorData
{
  /// <summary>Identifier of the inertial sensor from which this data was
  ///   received.
  /// </summary>
  unsigned int sensorId;
  /// <summary>A rotation matrix giving the orientation of the inertial
  ///   sensor.
  /// </summary>
  Matrix33 orientation;
  /// <summary>The confidence about the orientation.</summary>
  float confidence;
  /// <summary>Timing information for the frame.</summary>
  TimeInfo timeInfo;
};

/// <summary>The status of the inertial sensor dongle.</summary>
struct InertialSensorDongleStatus
{
  /// <summary>Boolean value indicating whether the dongle is available.</summary>
  bool available;
  /// <summary>Boolean value indicating whether the sensor data come from
  ///   playback of recorded videos.
  /// </summary>
  bool playback;
};

/// <summary>The status of an inertial sensor.</summary>
struct InertialSensorStatus
{
  /// <summary>Percentage of the inertial sensor's battery that is
  ///   charged.
  /// </summary>
  int batteryPercent;
  /// <summary>Boolean value indicating whether the server is currently
  ///   receiving data from this inertial sensor.
  /// </summary>
  bool streaming;
  /// <summary>Boolean value indicating whether the first button on this
  ///   inertial sensor is currently pressed.
  /// </summary>
  bool buttonState1;
  /// <summary>Boolean value indicating whether the second button on this
  ///   inertial sensor is currently pressed.
  /// </summary>
  bool buttonState2;
};

/// <summary>Values that represent cameras types.</summary>
enum Cameras
{
  /// <summary>1394 cameras.</summary>
  Cameras_1394,
  /// <summary>GigE cameras.</summary>
  Cameras_GigE,
  /// <summary>Max. Not an actual cameras type.</summary>
  CameraMax
};

} // end ns sdk2
} // end ns om

#include "om/sdk2/types.inl"

#endif //OM_SDK2_TYPES_H
