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
#ifndef OM_SDK2_SYSTEM_INFO_SERVICE_H
#define OM_SDK2_SYSTEM_INFO_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>System information service interface.</summary>
class ISystemInfoService : public IOMObject
{
public:
  /// <summary>Obtain a list of driver information for all devices of the
  ///   specified type on the server.
  /// </summary>
  ///
  /// <param name="deviceType">The type of device for which to obtain driver
  ///   information.
  /// </param>
  /// <param name="driverInfoList">Pointer to a smart pointer that will be set
  ///   to a newly allocated list of driver information, which will contain one
  ///   entry for each device of the specified type.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetDriverInfo(DeviceType deviceType,
                                    DriverInfoListPtr* driverInfoList) const = 0;
};

/// <summary>Smart pointer to a system information service interface.</summary>
typedef OMPtr<ISystemInfoService> SystemInfoServicePtr;

/// <summary>Constant smart pointer to a system information service
///   interface.
/// </summary>
typedef OMPtr<const ISystemInfoService> SystemInfoServiceConstPtr;

/// <summary>Create a system information service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created system information
///   service.
/// </returns>
SystemInfoServicePtr CreateSystemInfoService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ISystemInfoService* OMCALL _CreateSystemInfoService(
  const ClientPtr& client);
#endif

inline SystemInfoServicePtr CreateSystemInfoService(const ClientPtr& client)
{
  return SystemInfoServicePtr(_CreateSystemInfoService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_SYSTEM_INFO_SERVICE_H
