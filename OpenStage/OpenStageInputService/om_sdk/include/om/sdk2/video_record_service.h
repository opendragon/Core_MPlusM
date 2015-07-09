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
#ifndef OM_SDK2_VIDEO_RECORD_SERVICE_H
#define OM_SDK2_VIDEO_RECORD_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Video recording service interface.</summary>
class IVideoRecordService : public IOMObject
{
public:
  /// <summary>Start video recording.</summary>
  ///
  /// <param name="params">Parameters specifying video name and password.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL StartRecording(const VideoRecordParameters& params) = 0;

  /// <summary>Stop video recording.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL StopRecording() = 0;

  /// <summary>Get the current video recording state.</summary>
  ///
  /// <param name="state">Pointer to a video recording state value that will be
  ///   set to the current video recording state.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetState(VideoRecordState* state) const = 0;

  /// <summary>Get the default selected cameras that will be used for newly
  ///   recorded videos.
  /// </summary>
  ///
  /// <param name="selectedCameras">Pointer to a smart pointer that will be
  ///   set to a newly allocated Boolean list of size equal to the number of
  ///   cameras connected to the server.  Each Boolean value will be set to
  ///   true if the corresponding camera is selected or false if it is not
  ///   selected.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetDefaultSelectedCameras(
    BoolListPtr* selectedCameras) const = 0;

  /// <summary>Set the default selected cameras that will be used for newly
  ///   recorded videos.
  /// </summary>
  ///
  /// <param name="selectedCameras">Smart pointer to a Boolean list of size
  ///   equal to the number of cameras connected to the server.  Each camera
  ///   will be selected if the corresponding Boolean value is true or
  ///   deselected if it is false.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetDefaultSelectedCameras(
    const BoolListConstPtr& selectedCameras) = 0;
};

/// <summary>Smart pointer to a video recording service interface.</summary>
typedef OMPtr<IVideoRecordService> VideoRecordServicePtr;

/// <summary>Constant smart pointer to a video recording service interface.</summary>
typedef OMPtr<const IVideoRecordService> VideoRecordServiceConstPtr;

/// <summary>Create a video recording service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created video recording service.</returns>
VideoRecordServicePtr CreateVideoRecordService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IVideoRecordService* OMCALL _CreateVideoRecordService(
  const ClientPtr& client);
#endif

inline VideoRecordServicePtr CreateVideoRecordService(const ClientPtr& client)
{
  return VideoRecordServicePtr(_CreateVideoRecordService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_VIDEO_RECORD_SERVICE_H
