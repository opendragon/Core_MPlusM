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
#ifndef OM_SDK2_ACTOR_STREAM_MEMORY_H
#define OM_SDK2_ACTOR_STREAM_MEMORY_H

#include "om/sdk2/actor_stream.h"
#include "om/sdk2/stream_memory.h"

namespace om
{
namespace sdk2
{

/// <summary>Interface to store actor stream data in memory.</summary>
class IActorStreamMemory : public IStreamMemory<IActorDataList> {};

/// <summary>Smart pointer reference to an actor stream memory interface.</summary>
typedef OMPtr<IActorStreamMemory> ActorStreamMemoryPtr;

/// <summary>Constant smart pointer reference to an actor stream memory
///   interface.
/// </summary>
typedef OMPtr<const IActorStreamMemory> ActorStreamMemoryConstPtr;

/// <summary>Create an actor stream memory object.</summary>
///
/// <param name="actorStream">The actor stream from which to record data.</param>
///
/// <returns>Smart pointer to the newly created actor stream memory object.</returns>
ActorStreamMemoryPtr CreateActorStreamMemory(const ActorStreamPtr& actorStream);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorStreamMemory* OMCALL _CreateActorStreamMemory(
  const ActorStreamPtr& actorStream);
#endif

inline ActorStreamMemoryPtr CreateActorStreamMemory(const ActorStreamPtr& actorStream)
{
  return ActorStreamMemoryPtr(_CreateActorStreamMemory(actorStream));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_ACTOR_STREAM_H
