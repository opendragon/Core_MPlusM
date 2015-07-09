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
#ifndef OM_SDK2_INERTIAL_SENSOR_STREAM_H
#define OM_SDK2_INERTIAL_SENSOR_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/stream.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Intertial sensor stream interface.</summary>
class IInertialSensorStream : public IStreamRead<IInertialSensorDataList>
{
public:
  /// <summary>Add an inertial sensor to the list of sensors for which data
  ///   will be sent on this stream.
  /// </summary>
  ///
  /// <param name="sensorId">Identifier of the inertial sensor to
  ///   include.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IncludeSensor(unsigned int sensorId) = 0;

  /// <summary>Remove an inertial sensor to the list of sensors for which data
  ///   will be sent on this stream.
  /// </summary>
  ///
  /// <param name="sensorId">Identifier of the inertial sensor to
  ///   exclude.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL ExcludeSensor(unsigned int sensorId) = 0;
};

/// <summary>Smart pointer reference to an inertial sensor stream
///   interface.
/// </summary>
typedef OMPtr<IInertialSensorStream> InertialSensorStreamPtr;

/// <summary>Constant smart pointer reference to an inertial sensor stream
///   interface.
/// </summary>
typedef OMPtr<const IInertialSensorStream> InertialSensorStreamConstPtr;

/// <summary>Create an inertial sensor stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created inertial sensor stream.</returns>
InertialSensorStreamPtr CreateInertialSensorStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IInertialSensorStream* OMCALL _CreateInertialSensorStream(
  const ClientPtr& client);
#endif

inline InertialSensorStreamPtr CreateInertialSensorStream(const ClientPtr& client)
{
  return InertialSensorStreamPtr(_CreateInertialSensorStream(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_INERTIAL_SENSOR_STREAM_H
