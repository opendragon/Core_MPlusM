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
#ifndef OM_SDK2_INERTIAL_SENSOR_SERVICE_H
#define OM_SDK2_INERTIAL_SENSOR_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/list.h"

namespace om
{
namespace sdk2
{

/// <summary>Inertial sensor service interface.</summary>
class IInertialSensorService : public IOMObject
{
public:
  /// <summary>Query the status of the inertial sensor dongle connected to the
  ///   vision processor.
  /// </summary>
  ///
  /// <param name="status">Pointer to an inertial sensor dongle status data 
  ///   structure that will be filled with the status of the dongle.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetDongleStatus(InertialSensorDongleStatus* status) const = 0;

  /// <summary>Obtain a list of the serial numbers of all inertial sensors with
  ///   which the dongle connected to the vision processor can
  ///   communicate.
  /// </summary>
  ///
  /// <param name="result">Pointer to a smart pointer that will be set to a
  ///   newly allocated list of unsigned integers containing the sensor serial
  ///   numbers.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetAllSensorIds(UIntListPtr* result) const = 0;

  /// <summary>Calibrate the inertial sensors.</summary>
  ///
  /// <param name="sensorId">Identifier of a sensor that is currently pointing
  ///   to the north.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL CalibrateNorth(unsigned int sensorId) const = 0;

  /// <summary>Attempt to establish a connection with the specified inertial
  ///   sensor.  This operation is asynchronous.  Use
  ///   <see cref="IsDiscovering" /> to check when the operation has completed.
  /// </summary>
  ///
  /// <param name="sensorId">Identifier of the sensor with which to establish
  ///   a connection.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Discover(unsigned int sensorId) = 0;

  /// <summary>Attempt to establish a connection with all available inertial
  ///   sensors.  This operation is asynchronous.  Use
  ///   <see cref="IsDiscovering" /> to check when the operation has completed.
  /// </summary>
  ///
  /// <returns>True if succeeds or false if it fails.</returns>
  virtual bool OMCALL DiscoverAll() = 0;

  /// <summary>Break the connection with the specified inertial
  ///   sensor.
  /// </summary>
  ///
  /// <param name="sensorId">Identifier of the sensor with which to break the
  ///   connection.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Undiscover(unsigned int sensorId) = 0;

  /// <summary>Break the connection with all inertial sensors.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL UndiscoverAll() = 0;

  /// <summary>Check whether the server is in the process of attempting to
  ///   establish a connection to the inertial sensors.
  /// </summary>
  ///
  /// <param name="discovering">Pointer to a Boolean value that will be set to
  ///   true if the server is connecting to the inertial sensors.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IsDiscovering(bool *discovering) const = 0;

  /// <summary>Get the serial number of the inertial sensor with which the
  ///   server is currently attempting to establish a connection.
  /// </summary>
  ///
  /// <param name="sensorId">Pointer to an unsigned integer that will be set to
  ///   the serial number.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetDiscoveringSensorId(unsigned int* sensorId) const = 0;

  /// <summary>Abort inertial discovery if it is in progress.  This operation
  ///   is asynchronous.  Use <see cref="IsDiscovering" /> to check when
  ///   discovery has been aborted and is no longer in progress.
  /// </summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL AbortDiscovery() = 0;

  /// <summary>Query the status of the specified inertial sensor.</summary>
  ///
  /// <param name="sensorId">Identifier of the sensor of which to query the
  ///   status.
  /// </param>
  /// <param name="status">Pointer to an inertial sensor status data structure
  ///   that will be filled with the status of the specified sensor.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetSensorStatus(unsigned int sensorId,
                                      InertialSensorStatus* status) const = 0;
};

/// <summary>Smart pointer reference to an inertial sensor service interface.</summary>
typedef OMPtr<IInertialSensorService> InertialSensorServicePtr;

/// <summary>Constant smart pointer reference to an inertial sensor service
///   interface.
/// </summary>
typedef OMPtr<const IInertialSensorService> InertialSensorServiceConstPtr;

/// <summary>Create an inertial sensor service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created inertial sensor service.</returns>
InertialSensorServicePtr CreateInertialSensorService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IInertialSensorService* OMCALL _CreateInertialSensorService(
  const ClientPtr& client);
#endif

inline InertialSensorServicePtr CreateInertialSensorService(const ClientPtr& client)
{
  return InertialSensorServicePtr(_CreateInertialSensorService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_INERTIAL_SENSOR_SERVICE_H
