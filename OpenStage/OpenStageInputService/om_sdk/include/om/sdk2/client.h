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
#ifndef OM_SDK2_CLIENT_H
#define OM_SDK2_CLIENT_H

#include "om/sdk2/object.h"
#include "om/sdk2/string.h"
#include "om/sdk2/types.h"
#include "om/sdk2/list.h"

namespace om
{
namespace sdk2
{

/// <summary>Callback to receive notifications when a client loses write
///   access.  Override <see cref="OnLostWriteAccess" /> in your derived class.
///   Register an instance of your derived class by calling
///   <see cref="IClient::AddWriteAccessCallback" />.
/// </summary>
class WriteAccessCallback
{
public:
  /// <summary>Called when the client loses write access.</summary>
  ///
  /// <param name="otherIp">The IP address of the client that now has write
  ///   access.
  /// </param>
  virtual void OMCALL OnLostWriteAccess(const char* otherIp) = 0;

protected:
  ~WriteAccessCallback();
};

/// <summary>Callback to receive notifications when the image source is changed.
///   Override <see cref="OnImageSourceChanged" /> in your derived class.
///   Register an instance of your derived class by calling
///   <see cref="IClient::AddImageSourceCallback" />.
/// </summary>
class ImageSourceCallback
{
public:
  /// <summary>Called when the image source is changed.</summary>
  ///
  /// <param name="source">The new image source.</param>
  virtual void OMCALL OnImageSourceChanged(ImageSource source) = 0;

protected:
  ~ImageSourceCallback();
};

/// <summary>Client interface.</summary>
///
/// <remarks><para>The client is responsible for communication with one
///   OpenStage server.  Before calling any methods that interact with the
///   server, it is necessary to call <see cref="SetEndpoint" /> to set the
///   IP address and port for the server with which to communicate.  It is
///   possible to connect to a server while the server is still initializing.
///   In this case, most calls will fail until initialization is complete.  The
///   status of the initialization can be checked by calling
///   <see cref="IsInitializationComplete" />.
/// </para></remarks>
class IClient : public IOMObject
{
public:
  /// <summary>Register a write access callback with the client.</summary>
  ///
  /// <param name="callback">The callback to register.  Derive a class from
  ///   <see cref="WriteAccessCallback" /> and pass an instance of the derived
  ///   class.
  /// </param>
  virtual void OMCALL AddWriteAccessCallback(WriteAccessCallback* callback) = 0;

  /// <summary>Unregister a write access callback with the client.</summary>
  ///
  /// <param name="callback">The callback to unregister.  This callback should
  ///   have been previously registered using
  ///   <see cref="AddWriteAccessCallback" />.  If not, calling this function
  ///   will have no effect.
  /// </param>
  virtual void OMCALL RemoveWriteAccessCallback(WriteAccessCallback* callback) = 0;

  /// <summary>Set endpoint for network communication.</summary>
  ///
  /// <param name="ip">IP address of OpenStage server.</param>
  /// <param name="port">Port of OpenStage server.</param>
  ///
  /// <remarks><para>The port for most OpenStage servers is 15010.</para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetEndpoint(const char* ip, unsigned int port) = 0;

  /// <summary>Get endpoint for network communication.</summary>
  ///
  /// <param name="ip">Pointer to a smart pointer that will be set to a newly
  ///   allocated string containing the IP address of OpenStage server.
  /// </param>
  /// <param name="port">Pointer to an integer value that will be set to the
  ///   port of OpenStage server.
  /// </param>
  virtual void OMCALL GetEndpoint(StringPtr* ip, unsigned int* port) const = 0;

  /// <summary>Query if the connection to the endpoint is alive.</summary>
  ///
  /// <param name="ping">Boolean value indicating whether to ping the server to
  ///   check that it is still running.</param>
  ///
  /// <remarks><para>If <paramref name="ping" /> is set to false, there will be
  ///   no communication over the network as a result of the call.  This will
  ///   cause the function to return faster but will also cause it to return
  ///   true if the server has stopped responding but the client has not yet
  ///   disconnected.
  /// </para></remarks>
  ///
  /// <returns>True if the connection is alive or false if it is not.</returns>
  virtual bool OMCALL IsAlive(bool ping = true) const = 0;

  /// <summary>Request write access.</summary>
  ///
  /// <remarks><para>Write access is required to call non-const functions on a
  ///   client interface.  Only one client can have write access at a time.  If
  ///   this function is called while another client has write access, the
  ///   other client will lose write access to this client.  However, if the
  ///   other client is running an operation such as a background update or
  ///   video recording or the other client has obtained exclusive write access
  ///   by calling <see cref="SetExclusive" />, then it cannot be interrupted
  ///   until it finishes the operation or surrenders exclusive write access
  ///   by calling <see cref="SetNonExclusive" />.  In this case, calling this
  ///   method will fail.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL RequestWriteAccess() = 0;

  /// <summary>Request exclusive write access.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetExclusive() = 0;

  /// <summary>Surrender exclusive write access.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetNonExclusive() = 0;

  /// <summary>Query if this client has write access.</summary>
  ///
  /// <param name="writeAccess">Pointer to a Boolean value that will be set to
  ///   true if the client has write access.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL HasWriteAccess(bool* writeAccess) const = 0;

  /// <summary>Update all associated streams.</summary>
  ///
  /// <remarks><para>This call is blocking and will wait until either data is
  ///   available on at least one stream or the specified timeout has elapsed.
  /// </para></remarks>
  ///
  /// <param name="timeOutMs">The timeout in milliseconds.</param>
  ///
  /// <returns>True if data is received on at least one stream or false
  ///   if the timeout has elapsed.
  /// </returns>
  virtual bool OMCALL WaitAnyUpdateAll(int timeOutMs = 5000) = 0;

  /// <summary>Update all associated streams.</summary>
  ///
  /// <remarks><para>This call will always return immediately, failing if there
  ///   is no data available on any of the streams.
  /// </remarks>
  ///
  /// <returns>True if any stream received new data or false if not.</returns>
  virtual bool OMCALL WaitNoneUpdateAll() = 0;

  /// <summary>Break an active connection with the server.</summary>
  virtual void OMCALL Disconnect() = 0;

  /// <summary>Get the server OpenStage build version.</summary>
  ///
  /// <param name="version">Pointer to a version data structure that will be
  ///   filled with the server OpenStage version number.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetServerVersion(Version* version) const = 0;

  /// <summary>Save environment settings on the server.</summary>
  ///
  /// <remarks><para>By default, all settings are saved whenever a write-access
  ///   connection terminates.  This function saves environment settings
  ///   explicitly.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SaveSettings() = 0;

  /// <summary>Get the current list of system notifications.</summary>
  ///
  /// <remarks><para>This function is useful for a one-time sync to the current
  ///   list of notifications. In order to receive updates when a notification
  ///   is added or removed, create a notification stream.
  /// </para></remarks>
  ///
  /// <param name="notifications">Pointer to a smart pointer that will be set
  ///   to a newly allocated list of notifications.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetNotifications(NotificationListPtr* notifications) const = 0;

  /// <summary>Check if the server's initialization is complete yet.</summary>
  ///
  /// <param name="complete">Pointer to a Boolean that will be set to true if
  ///   the initialization is complete or false otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IsInitializationComplete(bool* complete) const = 0;

  /// <summary>Get a UUID identifying the instance of OpenStage running on the
  ///   server.  This UUID will change every time that OpenStage is restarted.
  /// </summary>
  ///
  /// <param name="instanceId">Pointer to a UUID that will be set to the
  ///   instance identifier.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetInstanceId(Uuid* instanceId) const = 0;

  /// <summary>Get the current timestamp from the server's internal clock.  This
  ///   timestamp is a 64-bit value representing the number of 100-nanosecond
  ///   intervals since January 1, 1601 (UTC).  This is the same value used in
  ///   the Win32 FILETIME structure and the same value used for
  ///   <see cref="TimeInfo::timestampProcessed" />.
  /// </summary>
  ///
  /// <param name="timestamp">Pointer to a 64-bit integer that will be set to
  ///   the current timestamp.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCurrentTimestamp(unsigned long long* timestamp) const = 0;

  /// <summary>Get the source of the images used for tracking.</summary>
  ///
  /// <param name="source">Pointer to an image source value to receive the
  ///   result.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetImageSource(ImageSource* source) const = 0;

  /// <summary>Set the source of the images used for tracking.</summary>
  ///
  /// <remarks><para>This value also controls which images will be sent through
  ///   <see cref="ICameraStream" /> and <see cref="ICameraStreamHD" />.
  /// </para></remarks>
  ///
  /// <param name="source">Image source value specifying whether live cameras
  ///   or recorded video should be used for tracking.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetImageSource(ImageSource source) = 0;

  /// <summary>Register an image source callback with the client.</summary>
  ///
  /// <param name="callback">The callback to register.  Derive a class from
  ///   <see cref="ImageSourceCallback" /> and pass an instance of the derived
  ///   class.
  /// </param>
  virtual void OMCALL AddImageSourceCallback(ImageSourceCallback* callback) = 0;

  /// <summary>Unregister an image source callback with the client.</summary>
  ///
  /// <param name="callback">The callback to unregister.  This callback should
  ///   have been previously register using
  ///   <see cref="AddImageSourceCallback" />.  If not, calling this function
  ///   will have no effect.
  /// </param>
  virtual void OMCALL RemoveImageSourceCallback(ImageSourceCallback* callback) = 0;
};

/// <summary>Smart pointer reference to a client interface.</summary>
typedef OMPtr<IClient> ClientPtr;

/// <summary>Constant smart pointer reference to a client interface.</summary>
typedef OMPtr<const IClient> ClientConstPtr;

/// <summary>Create a client.</summary>
///
/// <returns>Smart pointer to the newly created client.</returns>
ClientPtr CreateClient();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IClient* OMCALL _CreateClient();
#endif

// These functions are used internally to ensure that callbacks are properly
// destroyed when they go out of scope.
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC void OMCALL DeregisterWriteAccessCallback(
  WriteAccessCallback* callback);
extern "C" OMSDK2DECLSPEC void OMCALL DeregisterImageSourceCallback(
  ImageSourceCallback* callback);
#endif

inline WriteAccessCallback::~WriteAccessCallback()
{
  DeregisterWriteAccessCallback(this);
}

inline ImageSourceCallback::~ImageSourceCallback()
{
  DeregisterImageSourceCallback(this);
}

inline ClientPtr CreateClient()
{
  return ClientPtr(_CreateClient());
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_CLIENT_H
