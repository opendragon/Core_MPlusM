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
#ifndef OM_SDK2_SEGMENTATION_STREAM_HD_H
#define OM_SDK2_SEGMENTATION_STREAM_HD_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"
#include "om/sdk2/image_array.h"

namespace om
{
namespace sdk2
{

/// <summary>High-definition segmentation stream interface.</summary>
///
/// <remarks><para>The segmentation data are a probability mask indicating the
///   likelihood that a pixel in the raw camera image belongs to the foreground.
///   This stream will provide full-resolution images from a single camera.  To
///   obtain thumbnails from all cameras simultaneously, use
///   <see cref="ISegmentationStream" />.
/// </para></remarks>
class ISegmentationStreamHD : public IStreamRead<IImageArray>
{
public:
  /// <summary>Set the stream to provide segmentation images from the specified
  ///   camera.
  /// </summary>
  ///
  /// <remarks><para>Due to bandwidth restrictions, high-definition streaming is
  ///   supported for one camera at a time.
  /// </para></remarks>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL RequestCamera(int cameraIndex) = 0;
};

/// <summary>Smart pointer reference to a high-definition segmentation stream
///   interface.
/// </summary>
typedef OMPtr<ISegmentationStreamHD> SegmentationStreamHDPtr;

/// <summary>Constant smart pointer reference to a high-definition segmentation
///   stream interface.
/// </summary>
typedef OMPtr<const ISegmentationStreamHD> SegmentationStreamHDConstPtr;

/// <summary>Create a high-definition segmentation stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created high-definition segmentation
///   stream.
/// </returns>
SegmentationStreamHDPtr CreateSegmentationStreamHD(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ISegmentationStreamHD* OMCALL _CreateSegmentationStreamHD(
  const ClientPtr& client);
#endif

inline SegmentationStreamHDPtr CreateSegmentationStreamHD(const ClientPtr& client)
{
  return SegmentationStreamHDPtr(_CreateSegmentationStreamHD(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_CAMERA_STREAM_H
