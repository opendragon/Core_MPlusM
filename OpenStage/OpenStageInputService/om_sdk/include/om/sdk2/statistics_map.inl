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
namespace om
{
namespace sdk2
{

#ifndef DOXYGEN_INVOKED
inline StringPtr IStatisticsMapInternalIterator::GetKey() const
{
  return StringPtr(_GetKey());
}

inline OMPtr<IStatisticsMapInternalIterator> IStatisticsMapInternalIterator::Clone() const
{
  return IteratorPtr(_Clone());
}
#endif //DOXYGEN_INVOKED

inline StatisticsMapIterator IStatisticsMap::Begin()
{
  return IteratorPtr(_Begin());
}

inline StatisticsMapConstIterator IStatisticsMap::Begin() const
{
  return IteratorPtr(const_cast<IStatisticsMap*>(this)->_Begin());
}

inline StatisticsMapIterator IStatisticsMap::End()
{
  return StatisticsMapIterator();
}

inline StatisticsMapConstIterator IStatisticsMap::End() const
{
  return StatisticsMapConstIterator();
}

inline StatisticsMapConstIterator IStatisticsMap::CBegin() const
{
  return IteratorPtr(const_cast<IStatisticsMap*>(this)->_Begin());
}

inline StatisticsMapConstIterator IStatisticsMap::CEnd() const
{
  return StatisticsMapConstIterator();
}

inline StatisticsMapIterator::StatisticsMapIterator()
: m_it(),
  m_kvp()
{}

#ifndef DOXYGEN_INVOKED
inline StatisticsMapIterator::StatisticsMapIterator(const IteratorPtr& it)
: m_it(it),
  m_kvp()
{
  if (m_it.IsValid())
  {
    m_kvp = new KeyValuePair(m_it->GetKey(), *m_it->GetData());
  }
}
#endif

inline StatisticsMapIterator::StatisticsMapIterator(
  const StatisticsMapIterator& other)
: m_it(),
  m_kvp()
{
  *this = other;
}

inline const StatisticsMapIterator& StatisticsMapIterator::operator=(
  const StatisticsMapIterator& other)
{
  if (this != &other)
  {
    if (other.m_it.IsValid())
    {
      m_it = other.m_it->Clone();
    }
    else
    {
      m_it = NULL;
    }
    m_kvp = other.m_kvp.IsValid() ? new KeyValuePair(*other.m_kvp) : NULL;
  }
  return *this;
}

inline bool StatisticsMapIterator::operator==(
  const StatisticsMapIterator& other) const
{
  if (m_it.IsValid() && other.m_it.IsValid() &&
      m_kvp.IsValid() && other.m_kvp.IsValid())
  {
    return m_it->Equals(other.m_it);
  }
  else if (!m_kvp.IsValid() && !other.m_kvp.IsValid())
  {
    return true;
  }
  return false;
}

inline bool StatisticsMapIterator::operator!=(
  const StatisticsMapIterator& other) const
{
  return !(*this == other);
}

inline StatisticsMapIterator::KeyValuePair& StatisticsMapIterator::operator*() const
{
  return *m_kvp;
}

inline StatisticsMapIterator::KeyValuePair* StatisticsMapIterator::operator->() const
{
  return m_kvp.GetPtr();
}

inline StatisticsMapIterator& StatisticsMapIterator::operator++()
{
  if (m_kvp.IsValid() && m_it.IsValid())
  {
    if (m_it->Next())
    {
      m_kvp = new KeyValuePair(m_it->GetKey(), *m_it->GetData());
    }
    else
    {
      m_kvp = NULL;
    }
  }
  return *this;
}

inline StatisticsMapIterator StatisticsMapIterator::operator++(int)
{
  StatisticsMapIterator temp = *this;
  ++*this;
  return temp;
}

inline StatisticsMapConstIterator::StatisticsMapConstIterator()
: m_it(),
  m_kvp()
{}

#ifndef DOXYGEN_INVOKED
inline StatisticsMapConstIterator::StatisticsMapConstIterator(
  const IteratorPtr& it)
: m_it(it),
  m_kvp()
{
  if (m_it.IsValid())
  {
    m_kvp = new KeyValuePair(m_it->GetKey(), *m_it->GetData());
  }
}
#endif

inline StatisticsMapConstIterator::StatisticsMapConstIterator(
  const StatisticsMapConstIterator& other)
: m_it(),
  m_kvp()
{
  *this = other;
}

inline StatisticsMapConstIterator::StatisticsMapConstIterator(
  const StatisticsMapIterator& other)
: m_it(),
  m_kvp()
{
  *this = other;
}

inline const StatisticsMapConstIterator& StatisticsMapConstIterator::operator=(
  const StatisticsMapConstIterator& other)
{
  if (this != &other)
  {
    if (other.m_it.IsValid())
    {
      m_it = other.m_it->Clone();
    }
    else
    {
      m_it = NULL;
    }
    m_kvp = other.m_kvp.IsValid() ? new KeyValuePair(*other.m_kvp) : NULL;
  }
  return *this;
}

inline const StatisticsMapConstIterator& StatisticsMapConstIterator::operator=(
  const StatisticsMapIterator& other)
{
  if (other.m_it.IsValid())
  {
    m_it = other.m_it->Clone();
  }
  else
  {
    m_it = NULL;
  }
  m_kvp = other.m_kvp.IsValid() ? new KeyValuePair(*other.m_kvp) : NULL;
  return *this;
}

inline bool StatisticsMapConstIterator::operator==(
  const StatisticsMapConstIterator& other) const
{
  if (m_it.IsValid() && other.m_it.IsValid() &&
      m_kvp.IsValid() && other.m_kvp.IsValid())
  {
    return m_it->Equals(other.m_it);
  }
  else if (!m_kvp.IsValid() && !other.m_kvp.IsValid())
  {
    return true;
  }
  return false;
}

inline bool StatisticsMapConstIterator::operator!=(
  const StatisticsMapConstIterator& other) const
{
  return !(*this == other);
}

inline StatisticsMapConstIterator::KeyValuePair& StatisticsMapConstIterator::operator*() const
{
  return *m_kvp;
}

inline StatisticsMapConstIterator::KeyValuePair* StatisticsMapConstIterator::operator->() const
{
  return m_kvp.GetPtr();
}

inline StatisticsMapConstIterator& StatisticsMapConstIterator::operator++()
{
  if (m_kvp.IsValid() && m_it.IsValid())
  {
    if (m_it->Next())
    {
      m_kvp = new KeyValuePair(m_it->GetKey(), *m_it->GetData());
    }
    else
    {
      m_kvp = NULL;
    }
  }
  return *this;
}

inline StatisticsMapConstIterator StatisticsMapConstIterator::operator++(int)
{
  StatisticsMapConstIterator temp = *this;
  ++*this;
  return temp;
}

inline StatisticsMapPtr CreateStatisticsMap()
{
  return StatisticsMapPtr(_CreateStatisticsMap());
}

} // end ns sdk2
} // end ns om
