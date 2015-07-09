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
#ifndef OM_SDK2_ACTOR_POST_PROCESSING_FILTER_H
#define OM_SDK2_ACTOR_POST_PROCESSING_FILTER_H

#include "om/sdk2/object.h"
#include "om/sdk2/list.h"

namespace om
{
namespace sdk2
{

/// <summary>Actor post-processing filter interface.</summary>
class IActorPostProcessingFilter : public IOMObject
{
public:
  /// <summary>Filter stream data.</summary>
  ///
  /// <param name="dataIn">The raw actor data to be filtered.</param>
  ///
  /// <returns>The result of the filtering.</returns>
  ActorDataListPtr FilterData(const ActorDataListConstPtr& dataIn);

  /// <summary>Set the post-processing level.</summary>
  ///
  /// <param name="level">The new post-processing level.  This value should be
  ///   larger than 0.0 and less than or equal to 1.0.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetPostProcessingLevel(float level) = 0;

  /// <summary>Set the frame rate of the motion.</summary>
  ///
  /// <param name="framesPerSecond">Frame rate of the motion.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetMotionFrameRate(int framesPerSecond) = 0;

  /// <summary>Get the frame rate of the motion.</summary>
  ///
  /// <returns>Frame rate of the motion.</returns>
  virtual int OMCALL GetMotionFrameRate() const = 0;

  /// <summary>Enable foot stabilization.  Foot stabilization is enabled by
  ///   default.
  /// </summary>
  virtual void OMCALL EnableFootStabilization() = 0;

  /// <summary>Disable foot stabilization.</summary>
  virtual void OMCALL DisableFootStabilization() = 0;

  /// <summary>Enable foot IK.  Foot IK is enabled by default.</summary>
  virtual void OMCALL EnableFootIK() = 0;

  /// <summary>Disable foot IK.</summary>
  virtual void OMCALL DisableFootIK() = 0;

#ifndef DOXYGEN_INVOKED
protected:
  virtual IActorDataList* OMCALL _FilterData(
      const ActorDataListConstPtr& dataIn) = 0;
#endif
};

/// <summary>Smart pointer reference to an actor post-processing filter 
///   interface.
/// </summary>
typedef OMPtr<IActorPostProcessingFilter> ActorPostProcessingFilterPtr;

/// <summary>Constant smart pointer reference to an actor post-processing filter
///   interface.
/// </summary>
typedef OMPtr<const IActorPostProcessingFilter> ActorPostProcessingFilterConstPtr;

/// <summary>Create an actor post processing filter.</summary>
///
/// <returns>Smart pointer to the newly created filter.</returns>
ActorPostProcessingFilterPtr CreateActorPostProcessingFilter();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorPostProcessingFilter* OMCALL _CreateActorPostProcessingFilter();
#endif

inline ActorDataListPtr IActorPostProcessingFilter::FilterData(
    const ActorDataListConstPtr& dataIn)
{
  return ActorDataListPtr(_FilterData(dataIn));
}

inline ActorPostProcessingFilterPtr CreateActorPostProcessingFilter()
{
  return ActorPostProcessingFilterPtr(_CreateActorPostProcessingFilter());
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_ACTOR_POST_PROCESSING_FILTER_H
