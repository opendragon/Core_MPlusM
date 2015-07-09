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
#ifndef OM_SDK2_SEGMENTATION_STREAM_H
#define OM_SDK2_SEGMENTATION_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"
#include "om/sdk2/image_array.h"

namespace om
{
namespace sdk2
{

/// <summary>Segmentation stream interface.</summary>
///
/// <remarks><para>The segmentation data are a probability mask indicating the
///   likelihood that a pixel in the raw camera image belongs to the foreground.
///   This stream will provide thumbnails from all cameras.  To obtain
///   full-resolution images from a single camera, use
///   <see cref="ISegmentationStreamHD" />.
/// </para></remarks>
class ISegmentationStream : public IStreamRead<IImageArray> {};

/// <summary>Smart pointer reference to a segmentation stream interface.</summary>
typedef OMPtr<ISegmentationStream> SegmentationStreamPtr;

/// <summary>Constant smart pointer reference to a segmentation stream
///   interface.
/// </summary>
typedef OMPtr<const ISegmentationStream> SegmentationStreamConstPtr;

/// <summary>Create a segmentation stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created segmentation stream.</returns>
SegmentationStreamPtr CreateSegmentationStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ISegmentationStream* OMCALL _CreateSegmentationStream(
  const ClientPtr& client);
#endif

inline SegmentationStreamPtr CreateSegmentationStream(const ClientPtr& client)
{
  return SegmentationStreamPtr(_CreateSegmentationStream(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_CAMERA_STREAM_H
