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
#ifndef OM_SDK2_STREAM_READ_H
#define OM_SDK2_STREAM_READ_H

#include "om/sdk2/stream.h"
#include "om/util/conversion.h"

namespace om
{
namespace sdk2
{

/// <summary>Stream reading interface.</summary>
template<typename StreamDataType>
class IStreamRead : public IStream
{
  /// <summary>Type conversion test.  This type is used to ensure that the
  ///   proper type of pointer or smart pointer is used, depending on whether
  ///   raw data structures or objects are being streamed.
  /// </summary>
  typedef utils::Conversion<StreamDataType, IOMObject> OMObjectConvTest;

  /// <summary>Select stream types based on stream data type.  This type is used
  ///   to ensure that the proper type of pointer or smart pointer is used,
  ///   depending on whether raw data structures or objects are being streamed.
  /// </summary>
  template<typename T, int IOMObjectFlag> struct SelectType;
  template<typename T> struct SelectType<T, 0>
  {
    typedef T         GetType;
    typedef const T** GetInterfaceType;
  };
  template <typename T> struct SelectType<T, 1>
  {
    typedef OMPtr<T>        GetType;
    typedef OMPtr<const T>* GetInterfaceType;
  };

public:
  enum { IsOMObjectType = OMObjectConvTest::Exists };

  /// <summary>An alias representing the data type received.</summary>
  typedef typename SelectType<StreamDataType,
                              IsOMObjectType>::GetInterfaceType DataTypeGetInterface;
  /// <summary>An alias representing the data type received.</summary>
  typedef typename SelectType<StreamDataType,
                              IsOMObjectType>::GetType DataTypeGet;

  /// <summary>Obtain the data item retrieved the last time this stream was
  ///   updated.
  /// </summary>
  ///
  /// <param name="dataOut">Pointer to an object or data structure that will be
  ///   set to the data item.
  /// </param>
  ///
  /// <remarks><para>It is necessary to update streams before new data can be
  ///   retrieved using this function.  Streams are updated by calling
  ///   <see cref="Update" />, <see cref="IClient::WaitAnyUpdateAll" />, or
  ///   <see cref="IClient::WaitNoneUpdateAll" />.  Simply calling this function
  ///   without updating the streams will cause the same data item to be
  ///   obtained over and over again.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetData(DataTypeGetInterface dataOut) const = 0;

  /// <summary>Update this stream.</summary>
  ///
  /// <remarks><para>This call is blocking and will wait until either data is
  ///   available on the stream or the specified timeout has elapsed.
  /// </para></remarks>
  ///
  /// <param name="timeOutMs">The timeout in milliseconds.</param>
  ///
  /// <returns>True if data is received or false if the timeout has
  ///   elapsed.
  /// </returns>
  virtual bool OMCALL Update(int timeOutMs = 5000) = 0;
};

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_STREAM_READ_H
