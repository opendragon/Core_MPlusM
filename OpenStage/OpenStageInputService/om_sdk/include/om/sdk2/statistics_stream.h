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
#ifndef OM_SDK2_STATISTICS_STREAM_H
#define OM_SDK2_STATISTICS_STREAM_H

#include "om/sdk2/stream_read.h"
#include "om/sdk2/stream.h"
#include "om/sdk2/client.h"
#include "om/sdk2/types.h"
#include "om/sdk2/statistics_map.h"

namespace om
{
namespace sdk2
{

/// <summary>Statistics stream interface.</summary>
///
/// <remarks><para>Statistics is a key-value map.</para></remarks>
class IStatisticsStream : public IStreamRead<IStatisticsMap> {};

/// <summary>Smart pointer reference to a statistics stream interface.</summary>
typedef OMPtr<IStatisticsStream> StatisticsStreamPtr;

/// <summary>Constant smart pointer reference to a statistics stream
///   interface.
/// </summary>
typedef OMPtr<const IStatisticsStream> StatisticsStreamConstPtr;

/// <summary>Create a statistics stream.</summary>
///
/// <param name="client">The client.</param>
///
/// <returns>Smart pointer to the newly created statistics stream.</returns>
StatisticsStreamPtr CreateStatisticsStream(const ClientPtr& client);
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IStatisticsStream* OMCALL _CreateStatisticsStream(
  const ClientPtr& client);
#endif

inline StatisticsStreamPtr CreateStatisticsStream(const ClientPtr& client)
{
  return StatisticsStreamPtr(_CreateStatisticsStream(client));
}

} // end ns sdk2
} // end ns om

#endif //OM_SDK2_STATISTICS_STREAM_H
