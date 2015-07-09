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
namespace om
{
namespace sdk2
{

inline StringPtr IActorView::GetActorId()
{
  return StringPtr(_GetActorId());
}

inline JointTreeConstPtr IActorViewJoint::GetJointsAbsolute()
{
  return JointTreeConstPtr(_GetJointsAbsolute());
}

inline JointTreeConstPtr IActorViewJoint::GetJointsRelative()
{
  return JointTreeConstPtr(_GetJointsRelative());
}

inline SkeletonConstPtr IActorViewSegment::GetSkeletonAbsolute()
{
  return SkeletonConstPtr(_GetSkeletonAbsolute());
}

inline SkeletonConstPtr IActorViewSegment::GetSkeletonRelative()
{
  return SkeletonConstPtr(_GetSkeletonRelative());
}

inline ActorViewJointPtr CreateActorViewJoint()
{
  return ActorViewJointPtr(_CreateActorViewJoint());
}

inline ActorViewSegmentPtr CreateActorViewSegment()
{
  return ActorViewSegmentPtr(_CreateActorViewSegment());
}

} // end ns sdk2
} // end ns om
