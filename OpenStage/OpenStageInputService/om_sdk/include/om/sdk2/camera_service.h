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
#ifndef OM_SDK2_CAMERA_SERVICE_H
#define OM_SDK2_CAMERA_SERVICE_H

#include "om/sdk2/image_source_service.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"
#include "om/sdk2/list.h"

namespace om
{
namespace sdk2
{

/// <summary>Callback to receive notifications when there are changes to the
///   cameras.  Override <see cref="OnCameraChanging" /> and
///   <see cref="OnCameraChanged" /> in your derived class.  Register an
///   instane of your derived class by calling
///   <see cref="ICameraService::AddCameraChangeCallback" />.
/// </summary>
class CameraChangeCallback
{
public:
  /// <summary>Called when the cameras are restarting because a camera was
  /// added or removed.</summary>
  virtual void OMCALL OnCameraChanging() = 0;

  /// <summary>Called when the cameras have finished restarting.</summary>
  virtual void OMCALL OnCameraChanged() = 0;

protected:
  ~CameraChangeCallback();
};

/// <summary>Camera service interface.</summary>
class ICameraService : public IImageSourceService
{
public:
  /// <summary>Automatically adjust the exposure, including the white balance,
  ///   for all cameras.  This process will take place asynchronously over the
  ///   course of several seconds.  The status of this process can be queried by
  ///   calling <see cref="GetAutoExposureState" />.
  /// </summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetAutoExposure() = 0;

  /// <summary>Automatically adjust the white balance for all cameras.  This
  ///   process will take place asynchronously over the course of several
  ///   seconds.  The status of this process can be queried by calling
  ///   <see cref="GetAutoExposureState" />.
  /// </summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetAutoWhiteBalance() = 0;

  /// <summary>Set a property for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="prop">The property to set.</param>
  /// <param name="value">The new value of the property.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetProperty(int cameraIndex, CameraProperty prop,
                                  const CameraPropertyValue& value) = 0;

  /// <summary>Set a property for all cameras.</summary>
  ///
  /// <param name="prop">The property to set.</param>
  /// <param name="value">The new value of the property.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetPropertyAll(CameraProperty prop,
                                     const CameraPropertyValue& value) = 0;

  /// <summary>Set value A of a property for the specified camera.</summary>
  ///
  /// <remarks><para>This is faster than using <see cref="GetProperty" />
  ///   followed by <see cref="SetProperty" /> because it uses only one RPC
  ///   call.
  /// </para></remarks>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="prop">The property to set.</param>
  /// <param name="valueA">The new value A for the property.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetPropertyValueA(int cameraIndex, CameraProperty prop,
                                        float valueA) = 0;

  /// <summary>Set value B of a property for the specified camera.</summary>
  ///
  /// <remarks><para>This is faster than using <see cref="GetProperty" />
  ///   followed by <see cref="SetProperty" /> because it uses only one RPC
  ///   call.
  /// </para></remarks>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="prop">The property to set.</param>
  /// <param name="valueB">The new value B for the property.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetPropertyValueB(int cameraIndex, CameraProperty prop,
                                        float valueB) = 0;

  /// <summary>Get a property for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="prop">The property to get.</param>
  /// <param name="value">Pointer to a camera property value data structure that
  ///   will be set to the current value of the property.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetProperty(int cameraIndex,
                                  CameraProperty prop,
                                  CameraPropertyValue* value) const = 0;

  /// <summary>Get a property for all cameras.</summary>
  ///
  /// <param name="prop">The property to get.</param>
  /// <param name="values">Pointer to a smart pointer that will be set to a
  ///   newly allocated list of camera property values, one for each camera.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetPropertyAll(CameraProperty prop,
                                     CameraPropertyValueListPtr* values) const = 0;

  /// <summary>Get information about a camera property.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="prop">The property for which to get information.</param>
  /// <param name="propInfo">Pointer to a camera property information data
  ///   structure that will be filled with information on the specified property
  ///   for the specified camera.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetPropertyInfo(int cameraIndex, CameraProperty prop,
                                      CameraPropertyInfo* propInfo) const = 0;

  /// <summary>Check if the cameras are synchronized.</summary>
  ///
  /// <param name="synced">Pointer to a Boolean value that will be set to true
  ///   if the cameras are synchronized or false otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IsSynchronized(bool* synced) const = 0;

  /// <summary>Automatically calculate tracking limits.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL AutoTrackingLimits() = 0;

  /// <summary>Get the status of the last auto exposure request started using
  ///   either <see cref="SetAutoExposure" /> or
  ///   <see cref="SetAutoWhiteBalance" />.
  /// </summary>
  ///
  /// <param name="state">Pointer to an auto exposure state value that will be
  ///   set to the status of the last request.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetAutoExposureState(AutoExposureState* state) const = 0;

  /// <summary>Register a camera change callback.</summary>
  ///
  /// <param name="callback">The callback to register.  Derive a class from
  ///   <see cref="CameraChangeCallback" /> and pass an instance of the derived
  ///   class.
  /// </param>
  virtual void OMCALL AddCameraChangeCallback(CameraChangeCallback* callback) = 0;

  /// <summary>Unregister a camera change callback.</summary>
  ///
  /// <param name="callback">The callback to unregister.  This callback should
  ///   have been previously registered using
  ///   <see cref="CameraChangeCallback" />.  If not, calling this function will
  ///   have no effect.
  /// </param>
  virtual void OMCALL RemoveCameraChangeCallback(CameraChangeCallback* callback) = 0;

  /// <summary>Set an event marker.  Event markers can be used synchronize
  ///   OpenStage data with external events.
  /// </summary>
  ///
  /// <param name="eventMarkerVal">Flags passed into the event marker that will be
  ///   set on the frame. If multiple values happen within one frame, they will be
  ///   combined using bitwise OR.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  inline bool OMCALL SetEventMarker(int eventMarkerVal = 1) const
  // The actual implementation for SetEventMarker should go in vSetEventMarker implementation
  { return vSetEventMarker(eventMarkerVal); }; 

  /// <summary>Get the index of the camera with a given serial number.</summary>
  ///
  /// <param name="serialNumber">A camera serial number.</param>
  /// <param name="cameraIndex">Pointer to an integer that will be set to the
  ///   index of the camera with the specified serial number or a negative
  ///   number if there is no camera with that serial number.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraBySerialNumber(unsigned int serialNumber,
                                              int* cameraIndex) const = 0;

  /// <summary>Get the friendly name of the specified camera.</summary>
  ///
  /// <remarks>A friendly name stays the same as long as the same set of cameras
  ///   are connected to the server.
  /// </remarks>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="friendlyName">Pointer to a smart pointer that will be set to
  ///   a newly allocated string containing the friendly name.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraFriendlyName(int cameraIndex,
                                            StringPtr* friendlyName) const = 0;

  /// <summary>Get the bus number for the specified camera.</summary>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="busNumber">Pointer to an unsigned integer that will be set 
  ///   to the bus number of the camera.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraBusNumber(int cameraIndex, 
                                         unsigned int* busNumber) const = 0;

  /// <summary>Obtain a list of camera frame rates supported by the server.
  /// </summary>
  ///
  /// <param name="frameRates">Pointer to a smart pointer that will be set to
  ///   a newly allocated integer list containing the supported frame rates.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL EnumerateSupportedFrameRates(IntListPtr* frameRates) const = 0;

  /// <summary>Get the camera frame rate.</summary>
  ///
  /// <param name="frameRate">Pointer to an integer that will be set to the
  ///   frame rate (number of frames per second).</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetFrameRate(int* frameRate) const = 0; 

  /// <summary>Set the camera frame rate. Changing the frame rate requires 
  ///   stopping and re-starting all cameras.</summary>
  ///
  /// <param name="frameRate">The new number of frames per second.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetFrameRate(int frameRate) = 0;

  /// <summary>Create a camera exclusion list for a given frame rate.</summary>
  ///
  /// <param name="frameRate">The camera frame rate.</param>
  /// <param name="excludedCameraIndices">A list of integers that contains
  ///   indices of cameras to exclude.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetExclusionList(
      int frameRate,
      const IntListConstPtr& excludedCameraIndices) = 0;

  /// <summary>Get the camera exclusion list for a given frame rate.</summary>
  ///
  /// <param name="frameRate">The camera frame rate.</param>
  /// <param name="excludedCameraIndices">Pointer to a smart pointer that will 
  ///   be set to a newly allocated integer list containing indices of excluded
  ///   cameras.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetExclusionList(int frameRate,
                                       IntListPtr* excludedCameraIndices) = 0;

  /// <summary>Set the tracking limits.</summary>
  ///
  /// <param name="limits">An axis aligned box data structure that will be used
  ///   as the tracking limits.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetTrackingLimits(const AxisAlignedBox& limits) = 0;

  /// <summary>Get the metadata of the specified camera.</summary>
  ///
  /// <remarks>Metadata stays the same as long as the same set of cameras
  ///   are connected to the server.
  /// </remarks>
  ///
  /// <param name="cameraIndex">Zero-based index of the camera.</param>
  /// <param name="metaData">Pointer to a smart pointer that will be set to
  ///   a newly allocated string containing the metadata.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraInfo(int cameraIndex, StringPtr* metaData) const = 0;

  /// <summary>Get the cameras type.</summary>
  ///
  /// <param name="camerasType">Type of the cameras.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCamerasType(Cameras* camerasType) const = 0;

protected:

  /// <summary>Interface for SetEventMarker that the non-virtual version calls.</summary>
  ///
  /// <param name="eventMarker">Flags passed into the event marker that will be
  ///   set on the frame. If multiple values happen within one frame, they will be
  ///   combined using bitwise OR.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL vSetEventMarker(int eventMarker) const = 0;	
};

/// <summary>Smart pointer reference to a camera service interface.</summary>
typedef OMPtr<ICameraService> CameraServicePtr;

/// <summary>Constant smart pointer reference to a camera service interface.</summary>
typedef OMPtr<const ICameraService> CameraServiceConstPtr;

/// <summary>Creates a camera service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created camera service.</returns>
CameraServicePtr CreateCameraService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ICameraService* OMCALL _CreateCameraService(
  const ClientPtr& client);
#endif

// This function is used internally to ensure that a camera change callback is
// properly destroyed when it goes out of scope.
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC void OMCALL DeregisterCameraChangeCallback(
  CameraChangeCallback* callback);
#endif

inline CameraChangeCallback::~CameraChangeCallback()
{
  DeregisterCameraChangeCallback(this);
}

inline CameraServicePtr CreateCameraService(const ClientPtr& client)
{
  return CameraServicePtr(_CreateCameraService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_CAMERA_SERVICE_H
