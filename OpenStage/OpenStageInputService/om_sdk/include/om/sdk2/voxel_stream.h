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
#ifndef OM_SDK2_VOXEL_STREAM_H
#define OM_SDK2_VOXEL_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/stream.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"
#include "om/sdk2/image_array.h"

namespace om
{
namespace sdk2
{

/// <summary>Voxel stream interface.</summary>
class IVoxelStream : public IStreamRead<VoxelData> 
{
public:
  /// <summary>Set the view frustum for voxel image projection.</summary>
  ///
  /// <param name="frustum">Frustum of the virtual camera.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetViewFrustum(const Frustum& frustum) = 0;
};

/// <summary>Smart pointer reference to a voxel stream interface.</summary>
typedef OMPtr<IVoxelStream> VoxelStreamPtr;

/// <summary>Constant smart pointer reference to a voxel stream interface.</summary>
typedef OMPtr<const IVoxelStream> VoxelStreamConstPtr;

/// <summary>Create a voxel stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created voxel stream.</returns>
VoxelStreamPtr CreateVoxelStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IVoxelStream* OMCALL _CreateVoxelStream(
  const ClientPtr& client);
#endif

inline VoxelStreamPtr CreateVoxelStream(const ClientPtr& client)
{
  return VoxelStreamPtr(_CreateVoxelStream(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_VOXEL_STREAM_H
