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
#ifndef OM_SDK2_STATISTICS_MAP_H
#define OM_SDK2_STATISTICS_MAP_H

#include "om/sdk2/object.h"
#include "om/sdk2/string.h"
#include "om/sdk2/safe_auto_ptr.h"
#include "om/sdk2/safe_ref.h"
#include "om/sdk2/declspec.h"
#include <iterator>

namespace om
{
namespace sdk2
{

#ifndef DOXYGEN_INVOKED
/// <summary>An internal iterator for a statistics map, used by the SDK
///   implementation.
/// </summary>
class IStatisticsMapInternalIterator : public IOMObject
{
public:
  /// <summary>Smart pointer to an iterator of the same type as this iterator.</summary>
  typedef OMPtr<IStatisticsMapInternalIterator> IteratorPtr;

  /// <summary>Advance this iterator to the next statistic in the map.</summary>
  ///
  /// <returns>True if it succeeds or false if the end has been reached.</returns>
  virtual bool OMCALL Next() = 0;

  /// <summary>Get the key.</summary>
  ///
  /// <returns>The key.</returns>
  StringPtr GetKey() const;

  /// <summary>Get a pointer to the data.</summary>
  ///
  /// <returns>The data pointer.</returns>
  virtual float* OMCALL GetData() const = 0;

  /// <summary>Test if this iterator is considered equal to another.</summary>
  ///
  /// <param name="other">The other iterator to compare to this object.</param>
  ///
  /// <returns>True if the two iterators are equal or false otherwise.</returns>
  virtual bool OMCALL Equals(const IteratorPtr& other) const = 0;

  /// <summary>Create a new iterator that is equal to this one.</summary>
  ///
  /// <returns>A new iterator that is equal to this one.</returns>
  IteratorPtr Clone() const;

protected:
  virtual IString* OMCALL _GetKey() const = 0;
  virtual IStatisticsMapInternalIterator* OMCALL _Clone() const = 0;
};
#endif

class StatisticsMapIterator;
class StatisticsMapConstIterator;

/// <summary>A map used to hold statistics concerning the connection to the
///   server.
/// </summary>
class IStatisticsMap : public IOMObject
{
public:
  /// <summary>Add a statistic to the map.</summary>
  ///
  /// <param name="name">The name of the statistic.</param>
  /// <param name="value">The value of the statistic.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL AddStatistic(const char* name, float value) = 0;

  /// <summary>Get a statistic.</summary>
  ///
  /// <param name="name">The name of the statistic.</param>
  /// <param name="value">Pointer to a floating-point value that will be set to
  ///   the value of the statistic, if it exists.
  /// </param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetStatistic(const char* name, float* value) const = 0;

  /// <summary>Clear this object to its blank/initial state.</summary>
  virtual void OMCALL Clear() = 0;

  /// <summary>Copy the contents of another map.</summary>
  ///
  /// <param name="map">The map to copy.</param>
  virtual void OMCALL CopyMap(const OMPtr<const IStatisticsMap>& map) = 0;

  /// <summary>Get an iterator referencing the first item in the map.</summary>
  ///
  /// <returns>An iterator referencing the first item in the map.</returns>
  /**@{*/
  StatisticsMapIterator Begin();
  StatisticsMapConstIterator Begin() const;
  /**@}*/

  /// <summary>Get an iterator used to signal the end of iteration.</summary>
  ///
  /// <returns>An iterator used to signal the end of iteration.</returns>
  /**@{*/
  StatisticsMapIterator End();
  StatisticsMapConstIterator End() const;
  /**@}*/

  /// <summary>Get a constant iterator referencing the first item in the map.</summary>
  ///
  /// <returns>A constant iterator referencing the first item in the map.</returns>
  StatisticsMapConstIterator CBegin() const;

  /// <summary>Get a constant iterator used to signal the end of iteration.</summary>
  ///
  /// <returns>A constant iterator used to signal the end of iteration.</returns>
  StatisticsMapConstIterator CEnd() const;

#ifndef DOXYGEN_INVOKED
protected:
  virtual IStatisticsMapInternalIterator* OMCALL _Begin() = 0;

private:
  typedef OMPtr<IStatisticsMapInternalIterator> IteratorPtr;
#endif
};

/// <summary>Smart pointer reference to a statistics maps.</summary>
typedef OMPtr<IStatisticsMap> StatisticsMapPtr;

/// <summary>Constant smart pointer reference to a statistics maps.</summary>
typedef OMPtr<const IStatisticsMap> StatisticsMapConstPtr;

/// <summary>An iterator for a statistics map.</summary>
class StatisticsMapIterator
: public std::iterator<std::forward_iterator_tag, std::pair<const StringConstPtr, SafeRef<float> > >
{
public:
#ifndef DOXYGEN_INVOKED
  /// <summary>Smart pointer to an internal iterator of the same type as this
  ///   iterator.
  /// </summary>
  typedef OMPtr<IStatisticsMapInternalIterator> IteratorPtr;
#endif

  /// <summary>Key/value pair referenced by an iterator.</summary>
  typedef std::pair<const StringConstPtr, SafeRef<float> > KeyValuePair;

  StatisticsMapIterator();

#ifndef DOXYGEN_INVOKED
  /// <summary>Construct an iterator from an internal iterator.</summary>
  ///
  /// <param name="it">The internal iterator.</param>
  StatisticsMapIterator(const IteratorPtr& it);
#endif

  /// <summary>Construct an iterator from another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  StatisticsMapIterator(const StatisticsMapIterator& other);

  /// <summary>Set this iterator to be a copy of another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const StatisticsMapIterator& operator=(const StatisticsMapIterator& other);

  /// <summary>Check if this iterator is equal to another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>True if the iterators are equal or false otherwise.</returns>
  bool operator==(const StatisticsMapIterator& other) const;

  /// <summary>Check if this iterator is unequal to another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>True if the iterators are unequal or false otherwise.</returns>
  bool operator!=(const StatisticsMapIterator& other) const;

  /// <summary>Access the key/value pair referenced by this iterator.</summary>
  ///
  /// <returns>Reference to the key/value pair referenced by this iterator.</returns>
  KeyValuePair& operator*() const;

  /// <summary>Access the key/value pair referenced by this iterator.</summary>
  ///
  /// <returns>Pointer to the key/value pair referenced by this iterator.</returns>
  KeyValuePair* operator->() const;

  /// <summary>Advance to the next item in the map.</summary>
  ///
  /// <returns>Reference to this iterator.</returns>
  StatisticsMapIterator& operator++();

  /// <summary>Advance to the next item in the map.</summary>
  ///
  /// <returns>A copy of this iterator before advancing.</returns>
  StatisticsMapIterator operator++(int);

private:
  IteratorPtr m_it;
  SafeAutoPtr<KeyValuePair> m_kvp;

  friend class StatisticsMapConstIterator;
};

/// <summary>A constant iterator for a statistics map.</summary>
class StatisticsMapConstIterator
: public std::iterator<std::forward_iterator_tag, std::pair<const StringConstPtr, const float> >
{
public:
#ifndef DOXYGEN_INVOKED
  /// <summary>Smart pointer to an internal iterator of the same type as this
  ///   constant iterator.
  /// </summary>
  typedef OMPtr<IStatisticsMapInternalIterator> IteratorPtr;
#endif

  /// <summary>Key/value pair referenced by a constant iterator.</summary>
  typedef std::pair<const StringConstPtr, const float> KeyValuePair;

  StatisticsMapConstIterator();

#ifndef DOXYGEN_INVOKED
  /// <summary>Construct a constant iterator from an internal iterator.</summary>
  ///
  /// <param name="it">The internal iterator.</param>
  StatisticsMapConstIterator(const IteratorPtr& it);
#endif

  /// <summary>Construct a constant iterator from another constant iterator.</summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  StatisticsMapConstIterator(const StatisticsMapConstIterator& other);

  /// <summary>Construct a constant iterator from an iterator.</summary>
  ///
  /// <param name="other">The iterator.</param>
  StatisticsMapConstIterator(const StatisticsMapIterator& other);

  /// <summary>Set this constant iterator to be a copy of another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const StatisticsMapConstIterator& operator=(
    const StatisticsMapConstIterator& other);

  /// <summary>Set this constant iterator to be a copy of an iterator.</summary>
  ///
  /// <param name="other">The iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const StatisticsMapConstIterator& operator=(
    const StatisticsMapIterator& other);

  /// <summary>Check if this constant iterator is equal to another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>True if the constant iterators are equal or false
  ///   otherwise.
  /// </returns>
  bool operator==(const StatisticsMapConstIterator& other) const;

  /// <summary>Check if this constant iterator is unequal to another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>True if the constant iterators are unequal or false
  ///   otherwise.
  /// </returns>
  bool operator!=(const StatisticsMapConstIterator& other) const;

  /// <summary>Access the key/value pair referenced by this constant
  ///   iterator.
  /// </summary>
  ///
  /// <returns>Reference to the key/value pair referenced by this constant
  ///   iterator.
  /// </returns>
  KeyValuePair& operator*() const;

  /// <summary>Access the key/value pair referenced by this constant
  ///   iterator.
  /// </summary>
  ///
  /// <returns>Pointer to the key/value pair referenced by this constant
  ///   iterator.
  /// </returns>
  KeyValuePair* operator->() const;

  /// <summary>Advance to the next item in the map.</summary>
  ///
  /// <returns>Reference to this constant iterator.</returns>
  StatisticsMapConstIterator& operator++();

  /// <summary>Advance to the next item in the map.</summary>
  ///
  /// <returns>A copy of this constant iterator before advancing.</returns>
  StatisticsMapConstIterator operator++(int);

private:
  IteratorPtr m_it;
  SafeAutoPtr<KeyValuePair> m_kvp;
};

/// <summary>Creates a statistics map.</summary>
///
/// <returns>An empty statistics map.</returns>
StatisticsMapPtr CreateStatisticsMap();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IStatisticsMap* OMCALL _CreateStatisticsMap();
#endif

} // end ns sdk2
} // end ns om

#include "om/sdk2/statistics_map.inl"

#endif //OM_SDK2_STATISTICS_MAP_H
