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
#ifndef OM_SDK2_CAMERA_STREAM_H
#define OM_SDK2_CAMERA_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"
#include "om/sdk2/image_array.h"

namespace om
{
namespace sdk2
{

/// <summary>Camera stream interface.</summary>
///
/// <remarks><para>This stream will provide thumbnails from all cameras.  To
///   obtain full-resolution images from a single camera, use
///   <see cref="ICameraStreamHD" />.
/// </para></remarks>
class ICameraStream : public IStreamRead<IImageArray> {};

/// <summary>Smart pointer reference to a camera stream interface.</summary>
typedef OMPtr<ICameraStream> CameraStreamPtr;

/// <summary>Constant smart pointer reference to a camera stream
///   interface.
/// </summary>
typedef OMPtr<const ICameraStream> CameraStreamConstPtr;

/// <summary>Creates a camera stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created camera stream.</returns>
CameraStreamPtr CreateCameraStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ICameraStream* OMCALL _CreateCameraStream(
  const ClientPtr& client);
#endif

inline CameraStreamPtr CreateCameraStream(const ClientPtr& client)
{
  return CameraStreamPtr(_CreateCameraStream(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_CAMERA_STREAM_H
