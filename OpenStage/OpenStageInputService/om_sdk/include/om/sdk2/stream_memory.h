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
#ifndef OM_SDK2_STREAM_MEMORY_H
#define OM_SDK2_STREAM_MEMORY_H

#include "om/sdk2/stream_read.h"

namespace om
{
namespace sdk2
{

/// <summary>Base interface for objects used to record stream data and store it
///   in memory.  This is useful for persistent serialization.
/// </summary>
///
/// <remarks><para>While recording, all incoming stream data gets saved into
///   this object.
/// </para></remarks>
template <typename DataTypeGet>
class IStreamMemory : public IOMObject
{
public:
  /// <summary>An alias representing the data type recorded.</summary>
  typedef typename IStreamRead<DataTypeGet>::DataTypeGetInterface DataTypeGetInterface;

  /// <summary>Start the recording.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL StartRecording() = 0;
  
  /// <summary>Stop the recording.</summary>
  virtual void OMCALL StopRecording() = 0;

  /// <summary>Check whether the stream gets currently recorded.</summary>
  ///
  /// <returns>True if recording is in progress or false if it is not.</returns>
  virtual bool OMCALL IsRecording() const = 0;

  /// <summary>Get the number of data items that are stored in this object.</summary>
  ///
  /// <returns>The data count.</returns>
  virtual size_t OMCALL GetDataCount() const = 0;

  /// <summary>Get a data item.</summary>
  ///
  /// <param name="idx">Zero-based index of the item to get.</param>
  /// <param name="dataOut">Pointer to an object that will be set to the data
  ///   item.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetData(size_t idx, DataTypeGetInterface dataOut) const = 0;
};

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_STREAM_MEMORY_H
