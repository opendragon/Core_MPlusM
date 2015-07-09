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
#ifndef OM_SDK2_CALIBRATION_SERVICE_H
#define OM_SDK2_CALIBRATION_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Calibration service interface.</summary>
class ICalibrationService : public IOMObject
{
public:
  /// <summary>Start the calibration process.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Start() = 0;

  /// <summary>Stop the calibration process.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Stop() = 0;

  /// <summary>Advance to next state the calibration process.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Next() = 0;

  /// <summary>Revert to previous state the calibration process.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Prev() = 0;

  /// <summary>Get the current calibration state.</summary>
  ///
  /// <param name="state">Pointer to a calibration state value to receive the
  ///   result.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetState(CalibrationState* state) const = 0;

  /// <summary>Get the result of the last successful calibration.</summary>
  ///
  /// <param name="result">Pointer to a calibration result data structure to
  ///   receive the result.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetResult(CalibrationResult* result) const = 0;
};

/// <summary>Smart pointer reference to a calibration service interface.</summary>
typedef OMPtr<ICalibrationService> CalibrationServicePtr;

/// <summary>Constant smart pointer reference to a calibration service
///   interface.
/// </summary>
typedef OMPtr<const ICalibrationService> CalibrationServiceConstPtr;

/// <summary>Create a calibration service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created calibration service.</returns>
CalibrationServicePtr CreateCalibrationService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ICalibrationService* OMCALL _CreateCalibrationService(
  const ClientPtr& client);
#endif

inline CalibrationServicePtr CreateCalibrationService(const ClientPtr& client)
{
  return CalibrationServicePtr(_CreateCalibrationService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_CALIBRATION_SERVICE_H
