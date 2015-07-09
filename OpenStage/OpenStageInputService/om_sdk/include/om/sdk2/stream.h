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
#ifndef OM_SDK2_STREAM_H
#define OM_SDK2_STREAM_H

#include "om/sdk2/object.h"

namespace om
{
namespace sdk2
{

/// <summary>Base interface for all streams.</summary>
class IStream : public IOMObject
{
public:
  /// <summary>Start the stream.</summary>
  ///
  /// <returns>True if it succeeds, false if it fails.</returns>
  virtual bool OMCALL Start() = 0;
  
  /// <summary>Stop the stream.</summary>
  virtual void OMCALL Stop() = 0;

  /// <summary>Check whether the stream has been started.</summary>
  ///
  /// <returns>True if the stream has been started or false if it has not.</returns>
  virtual bool OMCALL IsStarted() const = 0;

  /// <summary>Set the buffer size.</summary>
  ///
  /// <remarks>
  ///   <para>The buffer size specifies the number of elements that are cached
  ///     before data are discarded.
  ///   </para>
  ///   <para>A small buffer size causes the stream to discard old elements in
  ///     favor of current ones, in the case that update rate of the client is
  ///     less than the publishing rate of the server.  A larger buffer prevents
  ///     this data loss but can add significant latency between the client and
  ///     the server.
  ///  </para>
  /// </remarks>
  ///
  /// <param name="size">The buffer size, which must be at least 1.</param>
  virtual void OMCALL SetBufferSize(int size) = 0;
};

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_STREAM_H
