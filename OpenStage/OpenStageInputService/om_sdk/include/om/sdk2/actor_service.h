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
#ifndef OM_SDK2_ACTOR_SERVICE_H
#define OM_SDK2_ACTOR_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Callback to receive notifications when there are changes to the
///   actor slots.  Override <see cref="OnActorSlotsChanged" /> in your derived
///   class.  Register an instance of your derived class by calling
///   <see cref="IActorService::AddActorSlotCallback" />.
/// </summary>
class ActorSlotCallback
{
public:
  /// <summary>Called whenever there is any change to the actor slots.</summary>
  virtual void OMCALL OnActorSlotsChanged() = 0;

protected:
  ~ActorSlotCallback();
};

/// <summary>Actor service interface.</summary>
class IActorService : public IOMObject
{
public:
  /// <summary>Reset the tracking of all actors.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL ResetAll() const = 0;

  /// <summary>Reset the tracking of a specific actor.</summary>
  ///
  /// <param name="actorId">Identifier of the actor for which to reset tracking.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Reset(const char* actorId) const = 0;

  /// <summary>Get the currently active actor slots.</summary>
  ///
  /// <param name="actorSlots">Pointer to a smart pointer that will be set to a
  ///   newly allocated list of actor slots containing the result.
  /// </param>
  ///
  /// <remarks><para>Actor slots define the classes and number of actors which
  ///   can be tracked by OpenStage in its current configuration.  Each actor
  ///   slot is defined by a name, a profile identifier, and a default skeleton
  ///   that represents the segment hierarchy as well as a default pose (such as
  ///   T-Stance).
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetActorSlots(ActorSlotListPtr* actorSlots) const = 0;

  /// <summary>Set the number of instances of each profile to acquire.</summary>
  ///
  /// <remarks><para>This function will create <paramref name="count" /> slots
  ///   for each profile.  If you wish to create slots for only some of the
  ///   available profiles or to create different numbers of slots for different
  ///   profiles, use <see cref="SetActorSlots" /> instead.
  /// </para></remarks>
  ///
  /// <param name="count">Number of slots to create for each profile.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetActorSlotsInstanceCount(int count) = 0;

  /// <summary>Set the actor slots.</summary>
  ///
  /// <param name="profileIds">List of profile identifiers for the actor slots.
  ///   A profile identifier may be specified more than once in this list in
  ///   order to create multiple actor slots for the same profile.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetActorSlots(const StringListConstPtr& profileIds) = 0;

  /// <summary>Get the actor occupying the specified slot.</summary>
  ///
  /// <param name="slotIndex">Zero-based index of the slot.</param>
  /// <param name="result">Pointer to a smart pointer that will be set to a
  ///   newly allocated string containing the identifier of the actor occupying
  ///   the slot.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetActorFromSlot(int slotIndex, StringPtr* result) const = 0;

  /// <summary>Add a single actor slot for the specified profile.</summary>
  ///
  /// <param name="profileId">Profile identifier for the actor slot.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL AddActorSlot(const char* profileId) = 0;

  /// <summary>Remove the specified actor slot.</summary>
  ///
  /// <param name="slotIndex">Zero-based index of the slot.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL RemoveActorSlot(int slotIndex) = 0;

  /// <summary>Change the profile for a given actor slot.</summary>
  ///
  /// <param name="slotIndex">Zero-based index of the slot.</param>
  /// <param name="profileId">New profile identifier for the slot.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetActorSlotProfile(int slotIndex, const char* profileId) = 0;

  /// <summary>Register an actor slot callback.</summary>
  ///
  /// <param name="callback">The callback to register.  Derive a class from
  ///   <see cref="ActorSlotCallback" /> and pass an instanced of the derived
  ///   class.
  /// </param>
  virtual void OMCALL AddActorSlotCallback(ActorSlotCallback* callback) = 0;
  
  /// <summary>Unregister an actor slot callback.</summary>
  ///
  /// <param name="callback">The callback to unregister.  This callback should
  ///   have been previously registered using
  ///   <see cref="AddActorSlotCallback" />.  If not, calling this function
  ///   will have no effect.
  /// </param>
  virtual void OMCALL RemoveActorSlotCallback(ActorSlotCallback* callback) = 0;

  /// <summary>Enable actor acquisition.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetAcquisitionActive() = 0;

  /// <summary>Disable actor acquisition.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetAcquisitionInactive() = 0;

  /// <summary>Get whether actor acquisition is currently enabled.</summary>
  ///
  /// <param name="active">Pointer to a Boolean value that will be set to true
  ///   if actor acquisition is enabled or false otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IsAcquisitionActive(bool* active) = 0;

  /// <summary>Save the current actor slots to file.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SaveActorSlots() = 0;

  /// <summary>Load the last saved actor slots from file.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL LoadActorSlots() = 0;

  /// <summary>Set the number of tracking passes per frame.</summary>
  ///
  /// <param name="count">Number of tracking passes per frame.</param>
  ///
  /// <remarks><para>This is an experimental feature to improve tracking of fast
  ///   motion.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetSameFramePassCount(int count) = 0;

  /// <summary>Set the maximum number of tracking passes per frame 
  ///   allowed.
  /// </summary>
  /// 
  /// <param name="count">Pointer to an integer that will be set to the maximum
  ///   number of passes allowed.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetSameFramePassCountMax(int* count) const = 0;
};

/// <summary>Smart pointer reference to an actor service interface.</summary>
typedef OMPtr<IActorService> ActorServicePtr;

/// <summary>Constant smart pointer reference to an actor service
///   interface.
/// </summary>
typedef OMPtr<const IActorService> ActorServiceConstPtr;

/// <summary>Create an actor service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created actor service.</returns>
ActorServicePtr CreateActorService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorService* OMCALL _CreateActorService(
  const ClientPtr& client);
#endif

// This function is used internally to ensure that an actor slot callback is
// properly destroyed when it goes out of scope.
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC void OMCALL DeregisterActorSlotCallback(
  ActorSlotCallback* callback);
#endif

inline ActorSlotCallback::~ActorSlotCallback()
{
  DeregisterActorSlotCallback(this);
}

inline ActorServicePtr CreateActorService(const ClientPtr& client)
{
  return ActorServicePtr(_CreateActorService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_ACTOR_SERVICE_H
