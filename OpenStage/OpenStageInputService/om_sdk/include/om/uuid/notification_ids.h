/*
 * =============================================================================
 *  Copyright(c) 2011-2012 Organic Motion, Inc. All Rights Reserved.
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
#ifndef OM_UUID_NOTIFICATION_IDS_H
#define OM_UUID_NOTIFICATION_IDS_H

#include "om/uuid/uuid.h"

namespace om
{
namespace uuid
{

/// <summary>Notification type indicating that there was a camera capture
/// error.</summary>
extern const Uuid NotificationType_CameraCapture;

/// <summary>Notification type indicating that a camera is missing its lens
/// calibration.</summary>
extern const Uuid NotificationType_LensCalibration;

/// <summary>Notification type indicating that a room calibration is
/// needed.</summary>
extern const Uuid NotificationType_RoomCalibration;

/// <summary>Notification type indicating that a background update is
/// needed.</summary>
extern const Uuid NotificationType_UpdateBackground;

/// <summary>Notification type indicating that an inertial sensor has a low
/// battery.</summary>
extern const Uuid NotificationType_InertialSensorBattery;

/// <summary>Notification type indicating that an inertial sensor is
/// missing.</summary>
extern const Uuid NotificationType_InertialSensorMissing;

/// <summary>Notification type indicating that actor acquisition is currently
/// inactive.</summary>
extern const Uuid NotificationType_ActorAcquisitionInactive;

/// <summary>Notification type indicating that there are no actor
/// slots.</summary>
extern const Uuid NotificationType_NoActorSlots;

#ifndef DOXYGEN_INVOKED
/// <summary>Notification type indicating that a there are too many monitors 
/// connected to a single GPU.</summary>
extern const Uuid NotificationType_TooManyMonitorsOnSingleGPU;
#endif

/// <summary>Notification type indicating that the server is low on disk space
/// for videos.</summary>
extern const Uuid NotificationType_LowDiskSpace;

/// <summary>Notification type indicating that one or more video frames could
/// not be read or decompressed, most likely due to video corruption.</summary>
extern const Uuid NotificationType_BadVideoFrames;

/// <summary>Notification type indicating that OpenStage has not been activated
/// on the server.</summary>
extern const Uuid NotificationType_Unlicensed;

/// <summary>Notification type indicating that the OpenStage license has expired
/// and needs to be renewed.</summary>
extern const Uuid NotificationType_LicenseExpired;

/// <summary>Notification type indicating that OpenStage is licensed for
/// demonstration purposes only.</summary>
extern const Uuid NotificationType_DemoLicense;

/// <summary>Notification type indicating that OpenStage may not be able to 
/// process camera images at the same frame rate as they come in.</summary>
extern const Uuid NotificationType_SlowFrameRate;

} // end ns uuid
} // end ns om

#endif //OM_UUID_NOTIFICATION_IDS_H
