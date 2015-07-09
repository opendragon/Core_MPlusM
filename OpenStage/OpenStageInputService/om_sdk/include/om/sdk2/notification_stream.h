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
#ifndef OM_SDK2_NOTIFICATION_STREAM_H
#define OM_SDK2_NOTIFICATION_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Notification stream interface.</summary>
///
/// <remarks><para>A system notification indicates some system status that
///   requires user action to correct or improve.  The stream provides
///   incremental updates to the list of notifications returned by
///   <see cref="IClient::GetNotifications" />.
/// </para></remarks>
class INotificationStream : public IStreamRead<Notification> {};

/// <summary>Smart pointer reference to a notification stream interface.</summary>
typedef OMPtr<INotificationStream> NotificationStreamPtr;

/// <summary>Constant smart pointer reference to a notification stream
///   interface.
/// </summary>
typedef OMPtr<const INotificationStream> NotificationStreamConstPtr;

/// <summary>Creates a notification stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created notification stream.</returns>
NotificationStreamPtr CreateNotificationStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC INotificationStream* OMCALL _CreateNotificationStream(
  const ClientPtr& client);
#endif

inline NotificationStreamPtr CreateNotificationStream(const ClientPtr& client)
{
  return NotificationStreamPtr(_CreateNotificationStream(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_NOTIFICATION_STREAM_H
