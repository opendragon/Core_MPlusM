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
#ifndef OM_SDK2_BACKGROUND_MODEL_SERVICE_H
#define OM_SDK2_BACKGROUND_MODEL_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"
#include "om/sdk2/image_array.h"

namespace om
{
namespace sdk2
{

/// <summary>Bounds on the shadow minimum value.</summary>
enum
{
  /// <summary>The lowest possible shadow minimum value.</summary>
  ShadowMin_Min = 0,
  /// <summary>The highest possible shadow minimum value.</summary>
  ShadowMin_Max = 60
};

/// <summary>Background model service interface.</summary>
///
/// <remarks><para>The background model is used to distinguish an actor from the
/// environment.</para></remarks>
class IBackgroundModelService : public IOMObject
{
public:
  /// <summary>Update the background model.</summary>
  ///
  /// <remarks><para>After this method is called and returns successfully, the
  ///   vision processor will obtain a new background model of the course of
  ///   seconds of video.  The status of this process can be queried by calling
  ///   <see cref="IsUpdating" /> or <see cref="GetUpdateProgress" />.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Update() = 0;

  /// <summary>Check whether background model update is currently in progress.</summary>
  ///
  /// <param name="updating">Pointer to Boolean value that will be set to true
  ///   if an update is in progress or false otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IsUpdating(bool* updating) const = 0;

  /// <summary>Obtain the fraction of the update process that has completed.</summary>
  ///
  /// <param name="progress">Pointer to a floating-point value that will be set
  ///   to the fraction of the update process that has completed, ranging from
  ///   0 to 1.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetUpdateProgress(float* progress) const = 0;

  /// <summary>Get the 3D background subtraction threshold.</summary>
  ///
  /// <param name="threshold">Pointer to a floating-point value that will be
  ///   set to the 3D background subtraction threshold.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetThreshold(float* threshold) const = 0;

  /// <summary>Set the 3D background subtraction threshold.</summary>
  ///
  /// <param name="threshold">The new 3D background subtraction threshold.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetThreshold(float threshold) = 0;

  /// <summary>Get the black threshold.</summary>
  ///
  /// <param name="threshold">Pointer to an integer value that will be set to
  ///   the black threshold.</param>
  ///
  /// <returns>True if it succeeds of false if it fails.</returns>
  virtual bool OMCALL GetBlackThreshold(int* threshold) const = 0;

  /// <summary>Set the black threshold.</summary>
  ///
  /// <param name="threshold">The new black threshold.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetBlackThreshold(int threshold) = 0;

  /// <summary>Get the shadow minimum.</summary>
  ///
  /// <param name="shadowMin">Pointer to an integer value that will be set to
  ///   the shadow minimum.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetShadowMin(int* shadowMin) const = 0;

  /// <summary>Set the shadow minimum.</summary>
  ///
  /// <param name="shadowMin">The new shadow minimum.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetShadowMin(int shadowMin) = 0;

  /// <summary>Get the current mask for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="imageArray">Pointer to a smart pointer that will be set to a
  ///   newly allocated image array of size 1 containing a monochrome image in
  ///   which each pixel is set to black if that pixel should be masked out or
  ///   white otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetMask(int cameraIndex, ImageArrayPtr* imageArray) const = 0;

  /// <summary>Set the current mask for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="imageArray">Smart pointer to an image array of size 1
  ///   containing a monochrome image in which each pixel is set to black if
  ///   that pixel should masked out or white otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetMask(int cameraIndex, const ImageArrayConstPtr& imageArray) = 0;

  /// <summary>Clear all masks.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL ClearMasks() = 0;
};

/// <summary>Smart pointer reference to a background model service interface.</summary>
typedef OMPtr<IBackgroundModelService> BackgroundModelServicePtr;

/// <summary>Constant smart pointer reference to a background model service
///   interface.
/// </summary>
typedef OMPtr<const IBackgroundModelService> BackgroundModelServiceConstPtr;

/// <summary>Create a background model service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created background model service.</returns>
BackgroundModelServicePtr CreateBackgroundModelService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IBackgroundModelService* OMCALL _CreateBackgroundModelService(
  const ClientPtr& client);
#endif

inline BackgroundModelServicePtr CreateBackgroundModelService(const ClientPtr& client)
{
  return BackgroundModelServicePtr(_CreateBackgroundModelService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_BACKGROUND_MODEL_SERVICE_H
