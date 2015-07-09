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
#ifndef OM_SDK2_UTIL_H
#define OM_SDK2_UTIL_H

#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Convert an SDK skeleton to an SDK joint tree in absolute
///   coordinates.
/// </summary>
///
/// <param name="skel">The skeleton in absolute coordinates.</param>
/// <param name="jt">Pointer to a smart pointer that will be set to a newly
///   allocated joint tree containing the result of the conversion in absolute
///   coordinates.
/// </param>
///
/// <returns>True if it succeeds or false if it fails.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL ConvertSkeletonToJointTreeAbsolute(
  const SkeletonConstPtr& skel, JointTreePtr* jt);

/// <summary>Convert an SDK skeleton to an SDK joint tree in relative
///   coordinates.
/// </summary>
///
/// <param name="skel">The skeleton in relative coordinates.</param>
/// <param name="jt">Pointer to a smart pointer that will be set to a newly
///   allocated joint tree containing the result of the conversion in relative
///   coordinates.
/// </param>
///
/// <returns>True if it succeeds or false if it fails.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL ConvertSkeletonToJointTreeRelative(
  const SkeletonConstPtr& skel, JointTreePtr* jt);

/// <summary>Convert an SDK skeleton from relative to absolute coordinates.</summary>
///
/// <param name="skel">The skeleton in relative coordinates.</param>
///
/// <returns>True if it succeeds or false if it fails.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL ConvertSkeletonToAbsolute(
  const SkeletonPtr& skel);

/// <summary>Convert an SDK skeleton from absolute to relative coordinates.</summary>
///
/// <param name="skel">The skeleton in absolute coordinates.</param>
///
/// <returns>True if it succeeds or false if it fails.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL ConvertSkeletonToRelative(
  const SkeletonPtr& skel);

/// <summary>Scale the segment lengths of a skeleton while preserving relative
///   orientations.
/// </summary>
///
/// <param name="skelPoseSrc">Skeleton representing the orientations (poses) to
///   retain.
/// </param>
/// <param name="skelSizeSrc">Skeleton with the target segment lengths.</param>
/// <param name="skelDst">Pointer to a smart pointer that will be set to a newly
///   allocated skeleton containing the result of the scaling.
/// </param>
///
/// <returns>True on success or false otherwise.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL ScaleSkeleton(const SkeletonConstPtr& skelPoseSrc,
                                                    const SkeletonConstPtr& skelSizeSrc,
                                                    SkeletonPtr* skelDst);

/// <summary>Extract the profile identifier and actor identifier from the
///   identifier stored in the <see cref="ActorData" /> structure.
/// </summary>
///
/// <param name="actorDataId">Identifier as received in the
///   <see cref="ActorData" />.
/// </param>
/// <param name="actorId">Pointer to a smart pointer that will be set to a newly
///   allocated string object containing the identifier for the actor.
/// </param>
/// <param name="profileId">Pointer to a smart pointer that will be set to a
///   newly allocated string object containing the identifier for the profile.
/// </param>
extern "C" OMSDK2DECLSPEC void OMCALL SplitActorDataId(
  const StringConstPtr& actorDataId, StringPtr* actorId, StringPtr* profileId);

/// <summary>Get a relative transformation from an absolute transformation.</summary>
///
/// <param name="parent">The parent transformation with respect to which to
///   calculate a relative transformation.
/// </param>
/// <param name="child">The child transformation to be converted to a relative
///   transformation.
/// </param>
/// <param name="result">Pointer to a 4 &times; 4 matrix that will be set to the
///   result, a transformation relative to the parent that is equivalent to the
///   child.
/// </param>
///
/// <returns>True if it succeeds or false if it fails.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL GetRelativeTransformation(
  const Matrix44& parent, const Matrix44& child, Matrix44* result);

/// <summary>Get an absolute transformation from a relative transformation.</summary>
///
/// <param name="parent">The parent transformation with respect to which the
///   child is relative.
/// </param>
/// <param name="child">The child transformation, specified relative to the
///   parent, to be converted to an absolute transformation.
/// </param>
/// <param name="result">Pointer to a 4 &times; 4 matrix that will be set to the
///   result, an absolute transformation that is equivalent to the child.
/// </param>
///
/// <returns>True if it succeeds or false if it fails.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL GetAbsoluteTransformation(
  const Matrix44& parent, const Matrix44& child, Matrix44* result);

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_UTIL_H
