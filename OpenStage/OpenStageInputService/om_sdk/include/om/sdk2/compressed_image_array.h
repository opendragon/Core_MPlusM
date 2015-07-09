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
#ifndef OM_SDK2_COMPRESSED_IMAGE_ARRAY_H
#define OM_SDK2_COMPRESSED_IMAGE_ARRAY_H

#include "om/sdk2/object.h"
#include "om/sdk2/declspec.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>A compressed image array.</summary>
class ICompressedImageArray : public IOMObject
{
public:
  /// <summary>Get a header containing information about the compressed image
  ///   array such as the number of images and compressed image file format.
  /// </summary>
  ///
  /// <param name="header">Pointer to a header data structure that will be
  ///   filled with information on this compressed image array.
  /// </param>
  virtual void OMCALL GetHeader(CompressedImageArrayHeader* header) const = 0;

  /// <summary>Get the data pointer for a specified image.</summary>
  ///
  /// <param name="i">Zero-based index of the image.</param>
  ///
  /// <returns>The data pointer or null if the index is out of range.</returns>
  /**@{*/
  virtual const unsigned char* OMCALL GetData(int i) const = 0;
  unsigned char* GetData(int i);
  /**@}*/

  /// <summary>Get the data size for the specified image.</summary>
  ///
  /// <param name="i">Zero-based index of the image.</param>
  ///
  /// <returns>The size in bytes of the data for the specified image.</returns>
  virtual size_t OMCALL GetDataSize(int i) const = 0;

  /// <summary>Set the data size for the specified image.</summary>
  ///
  /// <param name="i">Zero-based index of the image.</param>
  /// <param name="dataSize">The new data size.</param>
  ///
  /// <remarks><para>If the new data size is smaller than the old data size,
  ///   the image data will be truncated.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if the image index is out of
  ///   range.
  /// </returns>
  virtual bool OMCALL SetDataSize(int i, size_t dataSize) = 0;
};

/// <summary>Smart pointer reference to a compressed image array.</summary>
typedef OMPtr<ICompressedImageArray> CompressedImageArrayPtr;

/// <summary>Constant smart pointer reference to a compressed image
///   array.
/// </summary>
typedef OMPtr<const ICompressedImageArray> CompressedImageArrayConstPtr;

/// <summary>Create a compressed image array.</summary>
///
/// <param name="header">The header for the compressed image array.</param>
///
/// <returns>Smart pointer to the newly created image array.</returns>
CompressedImageArrayPtr CreateCompressedImageArray(
  const CompressedImageArrayHeader& header);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ICompressedImageArray* OMCALL _CreateCompressedImageArray(
  const CompressedImageArrayHeader& header);
#endif

inline unsigned char* ICompressedImageArray::GetData(int i)
{
  const ICompressedImageArray* pConstThis = this;
  return const_cast<unsigned char*>(pConstThis->GetData(i));
}

inline CompressedImageArrayPtr CreateCompressedImageArray(
  const CompressedImageArrayHeader& header)
{
  return CompressedImageArrayPtr(_CreateCompressedImageArray(header));
}

} // end ns sdk2
} // end ns om

#endif
