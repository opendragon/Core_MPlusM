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
#ifndef OM_SDK2_VIDEO_PLAYBACK_SERVICE_H
#define OM_SDK2_VIDEO_PLAYBACK_SERVICE_H

#include "om/sdk2/image_source_service.h"
#include "om/sdk2/client.h"
#include "om/sdk2/compressed_image_array.h"

namespace om
{
namespace sdk2
{

/// <summary>Video playback service interface.</summary>
class IVideoPlaybackService : public IImageSourceService
{
public:
  /// <summary>Open a video for playback on the server.</summary>
  ///
  /// <remarks><para>The video will be loaded asynchronously on the server.
  ///   This function will only fail if there was a failure sending the command
  ///   to load the video to the server.  To check whether the video has
  ///   finished loaded and, if so, whether it loaded successfully, call
  ///   <see cref="GetLoadState" />.
  /// </para></remarks>
  ///
  /// <param name="videoName">The name of the video to open.</param>
  /// <param name="password">The password for the video.  This parameter may be
  ///   null if the video is not password protected.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Open(const char* videoName, const char* password = NULL) = 0;

  /// <summary>Close any video that is currently open on the server.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Close() = 0;

  /// <summary>Get the status of the last load operation.</summary>
  ///
  /// <param name="loadState">Pointer to a value that will be set to the status
  ///   of the last load operation.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetLoadState(VideoLoadState* loadState) const = 0;

  /// <summary>Get the name of the video that is currently loaded.</summary>
  ///
  /// <param name="videoName">Pointer to a smart pointer that will be set a
  ///   newly allocated string containing the name of the currently load video
  ///   or an empty string if no video is loaded.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetVideoName(StringPtr* videoName) const = 0;

  /// <summary>Play the video.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Play() = 0;

  /// <summary>Play the video until the specified frame number number is reached
  ///   and then stop.
  /// </summary>
  ///
  /// <param name="frameNumber">Frame number at which to stop playback.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL PlayUntil(int frameNumber) = 0;

  /// <summary>Stop the video.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Stop() = 0;

  /// <summary>Get the current playback state.</summary>
  ///
  /// <param name="playbackState">Pointer to a value that will be set to the
  ///   current playback state.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetPlaybackState(VideoPlaybackState* playbackState) const = 0;

  /// <summary>Get the frame number of the current frame.</summary>
  ///
  /// <param name="frameNumber">Pointer to an integer that will be set to the
  ///   frame number.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCurrentFrame(int* frameNumber) const = 0;

  /// <summary>Move to the specified frame.</summary>
  ///
  /// <param name="frameNumber">Frame number of the frame to move to.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetCurrentFrame(int frameNumber) = 0;

  /// <summary>Get the number of frames in the video.</summary>
  ///
  /// <param name="frameCount">Pointer to an integer that will be set to the
  ///   number of frames in the video.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetFrameCount(int* frameCount) const = 0;

  /// <summary>Get the frame markers stored in the video metadata.</summary>
  ///
  /// <param name="markers">Pointer to a data structure that will be filled
  ///   with the frame markers.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetFrameMarkers(FrameMarkers* markers) const = 0;

  /// <summary>Set the frame markers stored in the video metadata.</summary>
  ///
  /// <param name="markers">The new frame markers.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetFrameMarkers(const FrameMarkers& markers) = 0;

  /// <summary>Get the actor slots stored in the video metadata.</summary>
  ///
  /// <param name="actorSlots">Pointer to a smart pointer to a string list that
  ///   will be set a list of profile identifiers for the actor slots.
  /// </param>
  ///
  /// <remarks><para>This function reads the actor slots in the video metadata,
  ///   which may be different from the actor slots used for acquisition and
  ///   tracking.  To obtain the actor slots used for acquisition and tracking,
  ///   use <see cref="IActorService::GetActorSlots" />.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetActorSlots(StringListPtr* actorSlots) const = 0;

  /// <summary>Set the actor slots stored in the video metadata.</summary>
  ///
  /// <param name="actorSlots">A string list of profile identifiers for the
  ///   actor slots.
  /// </param>
  ///
  /// <remarks><para>This function sets the actor slots stored in the video
  ///   metadata, which will not affect the actor slots used for acquisition and
  ///   tracking.  To set the actor slots used for acquisition and tracking, use
  ///   <see cref="IActorService::SetActorSlots" />.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetActorSlots(const StringListConstPtr& actorSlots) = 0;

  /// <summary>Download the current frame as an array of compressed image
  ///   files.
  /// </summary>
  ///
  /// <param name="format">The compressed image format in which to download
  ///   the frame.
  /// </param>
  /// <param name="centerPrincipalPoint">Boolean value indicated whether the
  ///   image should be translated so that the camera's principal point is
  ///   placed at the center of the image.
  /// </param>
  /// <param name="imageArray">Pointer to a smart pointer to a compressed
  ///   image array that will be set to hold the downloaded images.
  /// </param>
  ///
  /// <remarks><para>Only frames from those cameras that have been selected
  ///   using <see cref="SetSelectedCameras" /> will be downloaded.  The
  ///   compressed image array will always have its size equal to the total
  ///   number of cameras, but the images at indices corresponding to unselected
  ///   cameras will be empty.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCompressedImages(
    ImageFormat format, bool centerPrincipalPoint,
    CompressedImageArrayPtr* imageArray) const = 0;

  /// <summary>Get the selected cameras whose images will be downloaded when
  ///   <see cref="GetCompressedImages" /> is called.
  /// </summary>
  ///
  /// <param name="selectedCameras">Pointer to a smart pointer that will be
  ///   set to a newly allocated Boolean list of size equal to the number of
  ///   cameras with which the video was recorded.  Each Boolean value will be
  ///   set to true if the corresponding camera is selected or false if it is
  ///   not selected.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetSelectedCameras(
    BoolListPtr* selectedCameras) const = 0;

  /// <summary>Set the selected cameras whose images will be downloaded when
  ///   <see cref="SetCompressedImages" /> is called.
  /// </summary>
  ///
  /// <param name="selectedCameras">Smart pointer to a Boolean list of size
  ///   equal to the number of cameras with which the video was recorded.  Each
  ///   camera will be selected if the corresponding Boolean value is true or
  ///   deselected if it is false.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetSelectedCameras(
    const BoolListConstPtr& selectedCameras) = 0;

  /// <summary>Configure the system to keep all videos open as long as this
  ///   client is connected.  By default, OpenStage will automatically close
  ///   any video after the client that opened it disconnects.  Calling this
  ///   function will disable automatic closing of videos for as long as the
  ///   client that called it is connected.
  /// </summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL KeepVideosOpen() const = 0;

  /// <summary>Get the default actor slots stored in the video metadata.</summary>
  ///
  /// <param name="actorSlots">Pointer to a smart pointer to a string list that
  ///   will be set a list of profile identifiers for the default actor slots.
  /// </param>
  ///
  /// <remarks><para>This function reads the actor slots in the video metadata,
  ///   which may be different from the actor slots used for acquisition and
  ///   tracking.  To obtain the actor slots used for acquisition and tracking,
  ///   use <see cref="IActorService::GetActorSlots" />.
  /// </para></remarks>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetDefaultActorSlots(StringListPtr* actorSlots) const = 0;

  /// <summary>Get the camera frame rate when the video was recorded.</summary>
  ///
  /// <param name="frameRate">Pointer to an integer that will be set to the
  ///   frame rate (number of frames per second).</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetCameraFrameRate(int* frameRate) const = 0;
};

/// <summary>Smart pointer reference to a video playback service
///   interface.
/// </summary>
typedef OMPtr<IVideoPlaybackService> VideoPlaybackServicePtr;

/// <summary>Constant smart pointer reference to a video playback service
///   interface.
/// </summary>
typedef OMPtr<const IVideoPlaybackService> VideoPlaybackServiceConstPtr;

/// <summary>Create a video playback service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created video playback service.</returns>
VideoPlaybackServicePtr CreateVideoPlaybackService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IVideoPlaybackService* OMCALL _CreateVideoPlaybackService(
  const ClientPtr& client);
#endif

inline VideoPlaybackServicePtr CreateVideoPlaybackService(const ClientPtr& client)
{
  return VideoPlaybackServicePtr(_CreateVideoPlaybackService(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_VIDEO_PLAYBACK_SERVICE_H
