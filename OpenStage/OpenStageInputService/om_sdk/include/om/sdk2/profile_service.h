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
#ifndef OM_SDK2_PROFILE_SERVICE_H
#define OM_SDK2_PROFILE_SERVICE_H

#include "om/sdk2/object.h"
#include "om/sdk2/client.h"
#include "om/sdk2/list.h"

namespace om
{
namespace sdk2
{

/// <summary>Profile service interface.</summary>
class IProfileService : public IOMObject
{
public:
  /// <summary>Remove all profiles from the server.</summary>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL Reset() = 0;

  /// <summary>Add a profile to the server.</summary>
  ///
  /// <param name="profileXML">XML data specifying the profile.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL AddProfile(const char* profileXML) = 0;

  /// <summary>Remove the specified profile from the server.</summary>
  ///
  /// <param name="profileId">Identifier of the profile.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL RemoveProfile(const char* profileId) = 0;

  /// <summary>Get a list of the profile identifiers of all profiles currently
  /// on the server.</summary>
  ///
  /// <param name="profileIds">Pointer to a smart pointer that will be set to a
  ///   newly allocated string list with the profile identifiers.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetProfileIds(StringListPtr* profileIds) const = 0;
};

/// <summary>Smart pointer reference to a profile service interface.</summary>
typedef OMPtr<IProfileService> ProfileServicePtr;

/// <summary>Constant smart pointer reference to a profile service interface.</summary>
typedef OMPtr<const IProfileService> ProfileServiceConstPtr;

/// <summary>Create a profile service.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created profile service.</returns>
ProfileServicePtr CreateProfileService(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IProfileService* OMCALL _CreateProfileService(
  const ClientPtr& client);
#endif

inline ProfileServicePtr CreateProfileService(const ClientPtr& client)
{
  return ProfileServicePtr(_CreateProfileService(client));
}

} // end ns sdk2
} // end ns om

#endif
