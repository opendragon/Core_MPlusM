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
#ifndef OM_SDK2_ACTOR_VIEW_H
#define OM_SDK2_ACTOR_VIEW_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/string.h"
#include "om/sdk2/tree.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Actor view interface.</summary>
class IActorView : public IOMObject
{
public:
  /// <summary>Get the actor identifier from the actor data bound to this view.</summary>
  ///
  /// <returns>The actor identifier.</returns>
  StringPtr GetActorId();

  /// <summary>Get the frame identifier from the actor data bound to this view.</summary>
  ///
  /// <returns>The frame identifier.</returns>
  virtual int OMCALL GetFrameId() = 0;

  /// <summary>Get the timestamp from the actor data bound to this view.</summary>
  ///
  /// <returns>The timestamp.</returns>
  virtual double OMCALL GetTimestamp() = 0;

#ifndef DOXYGEN_INVOKED
protected:
  // This function is placed here, rather than at the end of the interface, in
  // order to preserve backward compatibility of the vtable.  DO NOT MOVE IT!
  virtual IString* OMCALL _GetActorId() = 0;
#endif

public:
  /// <summary>Clear this object to its blank/initial state.  After calling
  ///   this method, the other methods will all return null or zero.  It is
  ///   necessary to bind this actor view to actor data again before the other
  ///   methods will return meaningful data.
  /// </summary>
  virtual void OMCALL Clear() = 0;

  /// <summary>Bind the joint view to the specified actor data.  Once the joint
  ///   view has been bound, other methods can be called to obtain information
  ///   from the actor data.  This method makes an internal copy of the actor
  ///   data, so any changes to the actor data after binding will not be
  ///   reflected in the actor view.
  /// </summary>
  ///
  /// <param name="data">The actor data.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Bind(const ActorData& data) = 0;
};

/// <summary>Interface for an actor joint view.</summary>
class IActorViewJoint : public IActorView
{
public:
  /// <summary>Get a joint tree in absolute coordinates from the actor data
  ///   bound to this view.
  /// </summary>
  ///
  /// <returns>The joint tree.</returns>
  JointTreeConstPtr GetJointsAbsolute();

  /// <summary>Get a joint tree in relative coordinates from the actor data
  ///   bound to this view.
  /// </summary>
  ///
  /// <returns>The joint tree.</returns>
  JointTreeConstPtr GetJointsRelative();

#ifndef DOXYGEN_INVOKED
protected:
  virtual const ITree<Joint>* OMCALL _GetJointsAbsolute() = 0;
  virtual const ITree<Joint>* OMCALL _GetJointsRelative() = 0;
#endif
};

/// <summary>Smart pointer reference to an actor joint view interface.</summary>
typedef OMPtr<IActorViewJoint> ActorViewJointPtr;

/// <summary>Constant smart pointer reference to an actor joint view
///   interface.
/// </summary>
typedef OMPtr<const IActorViewJoint> ActorViewJointConstPtr;

/// <summary>Interface for an actor segment view.</summary>
class IActorViewSegment : public IActorView
{
public:
  /// <summary>Get a skeleton in absolute coordinates from the actor data bound
  ///   to this view.
  /// </summary>
  ///
  /// <returns>The skeleton in absolute coordinates.</returns>
  SkeletonConstPtr GetSkeletonAbsolute();

  /// <summary>Get a skeleton in relative coordinates from the actor data bound
  ///   to this view.
  /// </summary>
  ///
  /// <returns>The skeleton in relative coordinates.</returns>
  SkeletonConstPtr GetSkeletonRelative();

#ifndef DOXYGEN_INVOKED
protected:
  virtual const ITree<Segment>* OMCALL _GetSkeletonAbsolute() = 0;
  virtual const ITree<Segment>* OMCALL _GetSkeletonRelative() = 0;
#endif
};

/// <summary>Smart pointer reference to an actor segment view interface.</summary>
typedef OMPtr<IActorViewSegment> ActorViewSegmentPtr;

/// <summary>Constant smart pointer referene to an actor segment view
///   interface.
/// </summary>
typedef OMPtr<const IActorViewSegment> ActorViewSegmentConstPtr;

/// <summary>Create an actor joint view.</summary>
///
/// <returns>Smart pointer to the newly created actor joint view.</returns>
ActorViewJointPtr CreateActorViewJoint();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorViewJoint* OMCALL _CreateActorViewJoint();
#endif

/// <summary>Create an actor segment view.</summary>
///
/// <returns>Smart pointer to the newly created actor segment view.</returns>
ActorViewSegmentPtr CreateActorViewSegment();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorViewSegment* OMCALL _CreateActorViewSegment();
#endif

} // end ns sdk2
} // end ns om

#include "om/sdk2/actor_view.inl"

#endif //OM_SDK2_ACTOR_SERVICE_H
