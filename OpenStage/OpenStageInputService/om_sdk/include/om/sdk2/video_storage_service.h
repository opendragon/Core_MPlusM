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
#ifndef OM_SDK2_VIDEO_STORAGE_SERVICE_H
#define OM_SDK2_VIDEO_STORAGE_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"

namespace om
{
namespace sdk2
{

/// <summary>Video storage service interface.</summary>
class IVideoStorageService : public IOMObject
{
public:
  /// <summary>Obtain a list of all videos currently stored on the
  ///   server.
  /// </summary>
  ///
  /// <param name="videoNames">Pointer to a smart pointer that will be set to
  ///   a newly allocated string list containing the names of the videos.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL EnumerateVideos(StringListPtr* videoNames) const = 0;

  /// <summary>Delete a video from the server.</summary>
  ///
  /// <param name="videoName">Name of the video to delete.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL DeleteVideo(const char* videoName) = 0;

  /// <summary>Rename a video on the server.</summary>
  ///
  /// <param name="oldName">Old name of the video.</param>
  /// <param name="newName">New name of the video.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL RenameVideo(const char* oldName, const char* newName) = 0;

  /// <summary>Get the amount of disk space occupied on the server by the
  ///   specified video.
  /// </summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="videoSize">Pointer to a 64-bit integer to receive the
  ///   number of bytes occupied by the video.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoSize(const char* videoName,
                                   long long* videoSize) const = 0;

  /// <summary>Check whether the specified video is password protected.</summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="isProtected">Pointer to a Boolean value that will be set to
  ///   true if the video is password protected or false otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL IsPasswordProtected(const char* videoName,
                                          bool* isProtected) const = 0;

  /// <summary>Check if a password is correct for the specified video.</summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="password">The password to check.</param>
  /// <param name="isValid">Pointer to a Boolean value that will be set to true
  ///   if the password is correct or false otherwise.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL CheckPassword(const char* videoName,
                                    const char* password,
                                    bool* isValid) const = 0;

  /// <summary>Get the amount of free disk space available for videos on the
  ///   server.
  /// </summary>
  ///
  /// <param name="availableSpace">Pointer to a 64-bit integer to receive the
  ///   number of bytes of free disk space.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetAvailableSpace(long long* availableSpace) const = 0;

  /// <summary>Get the frame markers stored in the metadata of the specified
  ///   video.
  /// </summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="markers">Pointer to a data structure that will be filled
  ///   with the frame markers.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoFrameMarkers(const char* videoName,
                                           FrameMarkers* markers) const = 0;

  /// <summary>Get the actor slots stored in the metadata of the specified
  ///   video.
  /// </summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="actorSlots">Pointer to a smart pointer to a string list that
  ///   will be set to a list of profile identifiers for the actor slots.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoActorSlots(const char* videoName,
                                         StringListPtr* actorSlots) const = 0;

  /// <summary>Get the number of frames in the specified video.</summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="frameCount">Pointer to an integer that will be set to the
  ///   number of frames in the video.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoFrameCount(const char* videoName,
                                         int* frameCount) const = 0;

  /// <summary>Get the duration of a range of frames in the specified
  ///   video.
  /// </summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="startFrame">Index of the first frame.</param>
  /// <param name="endFrame">Index of the last frame.</param>
  /// <param name="duration">Pointer to a floating-point value to receive the
  ///   duration.
  /// </param>
  /// <param name="password">The password for the video.  This parameter may be
  ///   null if the video is not password protected.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoDuration(const char* videoName,
                                       int startFrame, int endFrame,
                                       double* duration,
                                       const char* password = NULL) const = 0;

  /// <summary>Set the actor slots stored in the metadata of the specified
  ///   video.
  /// </summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="actorSlots">Smart pointer to a string list that contains a 
  ///   list of profile identifiers for the actor slots.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetVideoActorSlots(const char* videoName,
                                         const StringListConstPtr& actorSlots) = 0;

  /// <summary>Set the default actor slots stored in the metadata of the 
  ///   specified video.
  /// </summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="actorSlots">Smart pointer to a string list that contains a 
  ///   list of profile identifiers for the actor slots.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetVideoDefaultActorSlots(const char* videoName,
                                                const StringListConstPtr& actorSlots) = 0;

  /// <summary>Get the date and time the specified video was recorded.</summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="seconds">Pointer to a 64-bit unsigned integer to receive 
  ///   the recorded time of the video. The time is represented as the number of
  ///   seconds elapsed since 00:00 hours, Jan 1, 1970.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoRecordedDate(const char* videoName,
                                           unsigned long long* seconds) const = 0;

  /// <summary>Download the specified video from the server to a local folder.
  ///   This function does not return until the download is finished or aborted.
  /// </summary>
  ///
  /// <param name="videoName">Name of the video.</param>
  /// <param name="pathToDownloadTo">Local path to store the downloaded video.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL DownloadVideo(const char* videoName,
                                    const char* pathToDownloadTo) = 0;

  /// <summary>Get the progress of the current video download.</summary>
  ///
  /// <param name="progress">Download progress, between 0 and 1.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoDownloadProgress(float* progress) = 0;

  /// <summary>Cancel the current video download.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL CancelVideoDownload() = 0;
};

/// <summary>Smart pointer to a video storage service interface.</summary>
typedef OMPtr<IVideoStorageService> VideoStorageServicePtr;

/// <summary>Constant smart pointer to a video storage service
///   interface.
/// </summary>
typedef OMPtr<const IVideoStorageService> VideoStroageServiceConstPtr;

/// <summary>Create a video storage service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created video storage service.</returns>
VideoStorageServicePtr CreateVideoStorageService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IVideoStorageService* OMCALL _CreateVideoStorageService(
  const ClientPtr& client);
#endif

/// <summary>Check if the specified video name is valid.</summary>
///
/// <param name="videoName">The video name to check.</param>
///
/// <returns>True if the video name is valid or false otherwise.</returns>
extern "C" OMSDK2DECLSPEC bool OMCALL IsValidVideoName(const char* videoName);

inline VideoStorageServicePtr CreateVideoStorageService(const ClientPtr& client)
{
  return VideoStorageServicePtr(_CreateVideoStorageService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_VIDEO_STORAGE_SERVICE_H
