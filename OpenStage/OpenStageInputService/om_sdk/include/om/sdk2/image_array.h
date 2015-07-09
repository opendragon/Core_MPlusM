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
#ifndef OM_SDK2_IMAGE_ARRAY_H
#define OM_SDK2_IMAGE_ARRAY_H

#include "om/sdk2/object.h"
#include "om/sdk2/declspec.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>An image array.</summary>
class IImageArray : public IOMObject
{
public:
  /// <summary>Get a header containing information about the image array such
  ///   as the number of images and the resolution of the images.
  /// </summary>
  ///
  /// <param name="header">Pointer to a header data structure that will be
  ///   filled with information on this image array.
  /// </param>
  virtual void OMCALL GetHeader(ImageArrayHeader* header) const = 0;

  /// <summary>Check if the image array is compatible with a given header.</summary>
  ///
  /// <param name="header">The header to check.</param>
  ///
  /// <returns>True if it is compatible or false otherwise.</returns>
  virtual bool OMCALL IsCompatible(const ImageArrayHeader& header) const = 0;

  /// <summary>Get the data pointer for a specified image.</summary>
  ///
  /// <param name="i">Zero-based index of the image.</param>
  ///
  /// <returns>The data pointer or null if the index is out of range.</returns>
  /**@{*/
  virtual const unsigned char* OMCALL GetData(int i) const = 0;
  unsigned char* GetData(int i);
  /**@}*/

  /// <summary>Get the data size.</summary>
  ///
  /// <returns>The size in bytes of the data for each image in the array.</returns>
  virtual size_t OMCALL GetDataSize() const = 0;

  /// <summary>Get the camera index associated with this image array.</summary>
  ///
  /// <returns>The camera index or a negative number if there is more than one
  ///   image in the array.
  /// </returns>
  virtual int OMCALL GetCameraIndex() const = 0;

  /// <summary>Set the camera index associated with this image array.</summary>
  ///
  /// <param name="cameraIndex">The camera index or a negative number to
  ///   indicate that there is more than one image in the array.
  /// </param>
  virtual void OMCALL SetCameraIndex(int cameraIndex) = 0;
};

/// <summary>Smart pointer reference to an image array.</summary>
typedef OMPtr<IImageArray> ImageArrayPtr;

/// <summary>Constant smart pointer reference to an image array.</summary>
typedef OMPtr<const IImageArray> ImageArrayConstPtr;

/// <summary>Create an image array.</summary>
///
/// <param name="header">The header for the image array.</param>
///
/// <returns>Smart pointer to the newly created image array.</returns>
ImageArrayPtr CreateImageArray(const ImageArrayHeader& header);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IImageArray* OMCALL _CreateImageArray(
  const ImageArrayHeader& header);
#endif

inline unsigned char* IImageArray::GetData(int i)
{
  const IImageArray* pConstThis = this;
  return const_cast<unsigned char*>(pConstThis->GetData(i));
}

inline ImageArrayPtr CreateImageArray(const ImageArrayHeader& header)
{
  return ImageArrayPtr(_CreateImageArray(header));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_IMAGE_ARRAY_H
