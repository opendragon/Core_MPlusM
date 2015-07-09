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
#ifndef OM_SDK2_ACTOR_STREAM_H
#define OM_SDK2_ACTOR_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Actor stream interface.</summary>
class IActorStream : public IStreamRead<IActorDataList> {};

/// <summary>Smart pointer reference to an actor stream interface.</summary>
typedef OMPtr<IActorStream> ActorStreamPtr;

/// <summary>Constant smart pointer reference to an actor stream
///   interface.
/// </summary>
typedef OMPtr<const IActorStream> ActorStreamConstPtr;

/// <summary>Create an actor stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created actor stream.</returns>
ActorStreamPtr CreateActorStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorStream* OMCALL _CreateActorStream(
  const ClientPtr& client);
#endif

inline ActorStreamPtr CreateActorStream(const ClientPtr& client)
{
  return ActorStreamPtr(_CreateActorStream(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_ACTOR_STREAM_H
