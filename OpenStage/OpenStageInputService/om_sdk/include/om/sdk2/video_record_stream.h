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
#ifndef OM_SDK2_VIDEO_RECORD_STREAM_H
#define OM_SDK2_VIDEO_RECORD_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Video recording stream interface.</summary>
class IVideoRecordStream : public IStreamRead<VideoRecordStatus> {};

/// <summary>Smart pointer reference to a video recording stream interface.</summary>
typedef OMPtr<IVideoRecordStream> VideoRecordStreamPtr;

/// <summary>Constant smart pointer reference to a video recording stream
///   interface.
/// </summary>
typedef OMPtr<const IVideoRecordStream> VideoRecordStreamConstPtr;

/// <summary>Create a video recording stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created video recording stream.</returns>
VideoRecordStreamPtr CreateVideoRecordStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IVideoRecordStream* OMCALL _CreateVideoRecordStream(
  const ClientPtr& client);
#endif

inline VideoRecordStreamPtr CreateVideoRecordStream(const ClientPtr& client)
{
  return VideoRecordStreamPtr(_CreateVideoRecordStream(client));
}

} // end ns sdk2
} // end ns om

#endif
