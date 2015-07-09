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
#ifndef OM_SDK2_ERROR_H
#define OM_SDK2_ERROR_H

#include "om/sdk2/string.h"

namespace om
{
namespace sdk2
{

/// <summary>Error codes indicating the type of error.</summary>
enum OMError
{
  /// <summary>The last operation succeeded.</summary>
  OMError_Success,
  /// <summary>A network error has occurred.</summary>
  OMError_Network,
  /// <summary>The operation has timed out.</summary>
  OMError_TimedOut,
  /// <summary>A camera error has occurred.</summary>
  OMError_Camera,
  /// <summary>A background subtraction error has occurred.</summary>
  OMError_BackgroundSubtraction,
  /// <summary>A motion recording error has occurred.</summary>
  OMError_MotionRecord,
  /// <summary>A null pointer was passed to the SDK.</summary>
  OMError_NullPointer,
  /// <summary>The image format is invalid.</summary>
  OMError_ImageFormat,
  /// <summary>A newer version of the SDK is required to connect to this server.</summary>
  OMError_ClientObsolete,
  /// <summary>Another operation is already in progress.</summary>
  OMError_OperationInProgress,
  /// <summary>The client already has exclusive write access.</summary>
  OMError_AlreadyExclusive,
  /// <summary>An invalid parameter was passed to the function.</summary>
  OMError_InvalidParameter,
  /// <summary>An invalid file name was specified.</summary>
  OMError_InvalidFileName,
  /// <summary>A video with the specified name already exists on the server.</summary>
  OMError_VideoAlreadyExists,
  /// <summary>Access is denied for the requested operation.</summary>
  OMError_AccessDenied,
  /// <summary>A room calibration error has occurred.</summary>
  OMError_RoomCalibration,
  /// <summary>A video recording error has occurred.</summary>
  OMError_VideoRecord,
  /// <summary>No data is available.</summary>
  OMError_NoDataAvailable,
  /// <summary>An invalid file extension was specified.</summary>
  OMError_InvalidFileExtension,
  /// <summary>The stream must be started before recording can be started.</summary>
  OMError_StreamNotStarted,
  /// <summary>The specified index was out of bounds.</summary>
  OMError_IndexOutOfBounds,
  /// <summary>The specified value was not found.</summary>
  OMError_ValueNotFound,
  /// <summary>The post-processing level must be between 0 and 1.</summary>
  OMError_LevelOutOfRange,
  /// <summary>The specified profile XML data is invalid.</summary>
  OMError_InvalidProfileXML,
  /// <summary>No profile with the specified identifier was found.</summary>
  OMError_ProfileNotFound,
  /// <summary>The requested operation is not supported by this server.</summary>
  OMError_OperationNotSupported,
  /// <summary>A network error occurred while trying to connect to the stream.</summary>
  OMError_StreamRegistration,
  /// <summary>A newer version of OpenStage must be installed on the server to
  ///   use this version of the SDK.</summary>
  OMError_ServerObsolete,
  /// <summary>The video storage configuration on the server is invalid.</summary>
  OMError_InvalidVideoStorage,
  /// <summary>The specified video was not found on the server.</summary>
  OMError_VideoNotFound,
  /// <summary>The video could not be deleted.</summary>
  OMError_FailedToDeleteVideo,
  /// <summary>The video could not be renamed.</summary>
  OMError_FailedToRenameVideo,
  /// <summary>The specified frame number is out of range.</summary>
  OMError_FrameNumberOutOfRange,
  /// <summary>Another video is already in the process of being loaded.</summary>
  OMError_VideoLoading,
  /// <summary>There is not enough disk space on the server.</summary>
  OMError_LowDiskSpace,
  /// <summary>Masks cannot be changed while in video mode.</summary>
  OMError_ChangeVideoMask,
  /// <summary>There is no video loaded on the server.</summary>
  OMError_NoVideoLoaded,
  /// <summary>An I/O error occurred on the server.</summary>
  OMError_IO,
  /// <summary>An invalid image format was specified.</summary>
  OMError_InvalidImageFormat,
  /// <summary>The size of the list passed to the function does not match the
  ///   number of cameras.
  /// </summary>
  OMError_CameraListSizeMismatch,
  /// <summary>The operation is not supported by reprocessors.</summary>
  OMError_ReprocessorUnsupported,
  /// <summary>This copy of OpenStage has not been activated.</summary>
  OMError_Unlicensed,
  /// <summary>An unexpected activation error has occurred.</summary>
  OMError_Activation,
  /// <summary>An invalid device type was specified.</summary>
  OMError_InvalidDeviceType,
  /// <summary>No inertial sensor with the specified identifier was found.</summary>
  OMError_InertialSensorNotFound,
  /// <summary>Inertial sensor discovery is already in progress.</summary>
  OMError_DiscoveryInProgress,
  /// <summary>The operation was canceled.</summary>
  OMError_OperationCanceled,
  /// <summary>The specified camera frame rate is not supported.</summary>
  OMError_CameraFrameRateUnsupported,
};

/// <summary>Gets the error code for the last OpenStage SDK error.</summary>
///
/// <remarks><para>The error code returned by this function will be set by all
///   functions in the OpenStage SDK that return a Boolean value indicating
///   success or failure.
/// </para></remarks>
///
/// <returns>The error code.</returns>
extern "C" OMSDK2DECLSPEC OMError OMCALL GetOMError();

/// <summary>Gets the description of the last OpenStage SDK error.</summary>
///
/// <remarks><para>This function provides text describing the error code
///   obtaining using <see cref="GetOMError" />.
/// </para></remarks>
///
/// <returns>The error description.</returns>
StringPtr GetOMErrorDescription();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IString* OMCALL _GetOMErrorDescription();
#endif

inline StringPtr GetOMErrorDescription()
{
  return StringPtr(_GetOMErrorDescription());
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_ERROR_H
