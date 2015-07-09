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
#ifndef OM_SDK2_IMAGE_SOURCE_SERVICE_H
#define OM_SDK2_IMAGE_SOURCE_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Base interface for services used to configure image sources.</summary>
class IImageSourceService : public IOMObject
{
public:
  /// <summary>Get the number of cameras that were detected on the server or
  ///   that were in use when the loaded video was recorded.
  /// </summary>
  ///
  /// <param name="numAttached">Pointer to an integer value that will be set to
  ///   the number of cameras.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetNumberOfCamerasAttached(int* numAttached) const = 0;

  /// <summary>Get the number of cameras that were successfully started on the
  ///   server or that were in use when the loaded video was recorded.
  /// </summary>
  ///
  /// <param name="numStarted">Pointer to an integer value that will be set to
  ///   the number of cameras.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetNumberOfCamerasStarted(int* numStarted) const = 0;

  /// <summary>Get the resolution of the camera or video images.</summary>
  ///
  /// <param name="width">Pointer to an integer value that will be set to the
  ///   width of the camera or video images.
  /// </param>
  /// <param name="height">Pointer to an integer value that will be set to the
  ///   height of the camera or video images.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetResolution(int* width, int* height) const = 0;

  /// <summary>Set all cameras to be active.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetAllActive() = 0;

  /// <summary>Set all cameras to be inactive.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetAllInactive() = 0;

  /// <summary>Set the specified camera to be active.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetActive(int cameraIndex) = 0;

  /// <summary>Set the specified camera to be inactive.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetInactive(int cameraIndex) = 0;

  /// <summary>Check if the specified camera is active.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="active">Pointer to a Boolean value that will be set to true
  ///   if the camera is active or false otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IsActive(int cameraIndex, bool* active) const = 0;

  /// <summary>Get the extrinsic matrix for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="extrinsic">Pointer to a 4 &times; 4 matrix that will be set
  ///   to the extrinsic matrix for the specified camera.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraExtrinsic(int cameraIndex, Matrix44* extrinsic) const = 0;

  /// <summary>Get the intrinsic matrix for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="intrinsic">Pointer to a 3 &times; 3 matrix that will be set
  ///   to the intrinsic matrix for the specified camera.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraIntrinsic(int cameraIndex, Matrix33* intrinsic) const = 0;

  /// <summary>Get the distortion parameters for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="distortion">Pointer to a distortion data structure that will
  ///   be filled with the distortion parameters for the specified camera.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraDistortion(int cameraIndex, Distortion* distortion) const = 0;

  /// <summary>Get the frustum for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="nearZ">The near <i>z</i>-coordinate.</param>
  /// <param name="farZ">The far <i>z</i>-coordinate.</param>
  /// <param name="frustum">Pointer to a frustum data structure that will be set
  ///   to the frustum for the specified camera with the specified
  ///   <i>z</i>-coordinates.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraFrustum(int cameraIndex, float nearZ, float farZ,
                                       Frustum* frustum) const = 0;

  /// <summary>Get the tracking limits.</summary>
  ///
  /// <param name="limits">Pointer to an axis aligned box data structure that
  ///   will be filled with the tracking limits.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetTrackingLimits(AxisAlignedBox* limits) const = 0;
};

/// <summary>Smart pointer reference to an image source service interface.</summary>
typedef OMPtr<IImageSourceService> ImageSourceServicePtr;

/// <summary>Constant smart pointer reference to an image source service
///   interface.
/// </summary>
typedef OMPtr<const IImageSourceService> ImageSourceServiceConstPtr;

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_IMAGE_SOURCE_SERVICE_H
