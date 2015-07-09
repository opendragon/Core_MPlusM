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

template<typename T>
inline T* IList<T>::ElementAt(size_t i)
{
  const IList<T>* pConstThis = this;
  return const_cast<T*>(pConstThis->ElementAt(i));
}

template<typename T>
inline T IList<T>::GetAt(size_t i) const
{
  T result;
  GetAt(i, &result);
  return result;
}

template<typename T>
inline ListIterator<T> IList<T>::Begin()
{
  AddRef();
  return ListIterator<T>(OMPtr<IList<T> >(this), 0);
}

template<typename T>
inline ListConstIterator<T> IList<T>::Begin() const
{
  AddRef();
  return ListConstIterator<T>(OMPtr<const IList<T> >(this), 0);
}

template<typename T>
inline ListIterator<T> IList<T>::End()
{
  AddRef();
  return ListIterator<T>(OMPtr<IList<T> >(this), GetSize());
}

template<typename T>
inline ListConstIterator<T> IList<T>::End() const
{
  AddRef();
  return ListConstIterator<T>(OMPtr<const IList<T> >(this), GetSize());
}

template<typename T>
inline ListConstIterator<T> IList<T>::CBegin() const
{
  AddRef();
  return ListConstIterator<T>(OMPtr<const IList<T> >(this), 0);
}

template<typename T>
inline ListConstIterator<T> IList<T>::CEnd() const
{
  AddRef();
  return ListConstIterator<T>(OMPtr<const IList<T> >(this), GetSize());
}

template<typename T>
inline std::reverse_iterator<ListIterator<T> > IList<T>::RBegin()
{
  return std::reverse_iterator<ListIterator<T> >(End());
}

template<typename T>
inline std::reverse_iterator<ListConstIterator<T> > IList<T>::RBegin() const
{
  return std::reverse_iterator<ListConstIterator<T> >(End());
}

template<typename T>
inline std::reverse_iterator<ListIterator<T> > IList<T>::REnd()
{
  return std::reverse_iterator<ListIterator<T> >(Begin());
}

template<typename T>
inline std::reverse_iterator<ListConstIterator<T> > IList<T>::REnd() const
{
  return std::reverse_iterator<ListConstIterator<T> >(Begin());
}

template<typename T>
inline std::reverse_iterator<ListConstIterator<T> > IList<T>::CRBegin() const
{
  return std::reverse_iterator<ListConstIterator<T> >(End());
}

template<typename T>
inline std::reverse_iterator<ListConstIterator<T> > IList<T>::CREnd() const
{
  return std::reverse_iterator<ListConstIterator<T> >(Begin());
}

inline void IListExtra<StringPtr>::Add(const char* item)
{
  Add(CreateString(item));
}

template<typename T>
inline ListIterator<T>::ListIterator()
: m_list(),
  m_pos(0)
{}

template<typename T>
inline ListIterator<T>::ListIterator(const ListPtr& list, size_t pos)
: m_list(list),
  m_pos(pos)
{}

template<typename T>
inline ListIterator<T>::ListIterator(const ListIterator& other)
: m_list(other.m_list),
  m_pos(other.m_pos)
{}

template<typename T>
inline const ListIterator<T>& ListIterator<T>::operator=(
  const ListIterator& other)
{
  m_list = other.m_list;
  m_pos = other.m_pos;
  return *this;
}

template<typename T>
inline bool ListIterator<T>::operator==(const ListIterator& other) const
{
  return m_list == other.m_list && m_pos == other.m_pos;
}

template<typename T>
inline bool ListIterator<T>::operator!=(const ListIterator& other) const
{
  return !(*this == other);
}

template<typename T>
inline T& ListIterator<T>::operator*() const
{
  return *m_list->ElementAt(m_pos);
}

template<typename T>
inline T* ListIterator<T>::operator->() const
{
  return m_list->ElementAt(m_pos);
}

template<typename T>
inline ListIterator<T>& ListIterator<T>::operator++()
{
  ++m_pos;
  return *this;
}

template<typename T>
inline ListIterator<T> ListIterator<T>::operator++(int)
{
  ListIterator temp = *this;
  ++*this;
  return temp;
}

template<typename T>
inline ListIterator<T>& ListIterator<T>::operator--()
{
  --m_pos;
  return *this;
}

template<typename T>
inline ListIterator<T> ListIterator<T>::operator--(int)
{
  ListIterator temp = *this;
  --*this;
  return temp;
}

template<typename T>
inline bool ListIterator<T>::operator<(const ListIterator& other) const
{
  return m_list == other.m_list && m_pos < other.m_pos;
}

template<typename T>
inline bool ListIterator<T>::operator<=(const ListIterator& other) const
{
  return m_list == other.m_list && m_pos <= other.m_pos;
}

template<typename T>
inline bool ListIterator<T>::operator>(const ListIterator& other) const
{
  return m_list == other.m_list && m_pos > other.m_pos;
}

template<typename T>
inline bool ListIterator<T>::operator>=(const ListIterator& other) const
{
  return m_list == other.m_list && m_pos >= other.m_pos;
}

template<typename T>
inline ListIterator<T>& ListIterator<T>::operator+=(ptrdiff_t i)
{
  m_pos += i;
  return *this;
}

template<typename T>
inline ListIterator<T> ListIterator<T>::operator+(ptrdiff_t i) const
{
  return ListIterator(m_list, m_pos + i);
}

template<typename T>
inline ListIterator<T>& ListIterator<T>::operator-=(ptrdiff_t i)
{
  m_pos -= i;
  return *this;
}

template<typename T>
inline ListIterator<T> ListIterator<T>::operator-(ptrdiff_t i) const
{
  return ListIterator(m_list, m_pos - i);
}

template<typename T>
inline ptrdiff_t ListIterator<T>::operator-(const ListIterator& other) const
{
  return m_pos - other.m_pos;
}

template<typename T>
inline T& ListIterator<T>::operator[](ptrdiff_t i) const
{
  return *m_list->ElementAt(m_pos + i);
}

template<typename T>
ListIterator<T> operator+(ptrdiff_t i, const ListIterator<T>& itr)
{
  return itr + i;
}

template<typename T>
inline ListConstIterator<T>::ListConstIterator()
: m_list(),
  m_pos(0)
{}

template<typename T>
inline ListConstIterator<T>::ListConstIterator(const ListConstPtr& list, size_t pos)
: m_list(list),
  m_pos(pos)
{}

template<typename T>
inline ListConstIterator<T>::ListConstIterator(const ListConstIterator& other)
: m_list(other.m_list),
  m_pos(other.m_pos)
{}

template<typename T>
inline ListConstIterator<T>::ListConstIterator(const ListIterator<T>& other)
: m_list(other.m_list),
  m_pos(other.m_pos)
{}

template<typename T>
inline const ListConstIterator<T>& ListConstIterator<T>::operator=(
  const ListConstIterator& other)
{
  m_list = other.m_list;
  m_pos = other.m_pos;
  return *this;
}

template<typename T>
inline const ListConstIterator<T>& ListConstIterator<T>::operator=(
  const ListIterator<T>& other)
{
  m_list = other.m_list;
  m_pos = other.m_pos;
  return *this;
}

template<typename T>
inline bool ListConstIterator<T>::operator==(const ListConstIterator& other) const
{
  return m_list == other.m_list && m_pos == other.m_pos;
}

template<typename T>
inline bool ListConstIterator<T>::operator!=(const ListConstIterator& other) const
{
  return !(*this == other);
}

template<typename T>
inline const T& ListConstIterator<T>::operator*() const
{
  return *m_list->ElementAt(m_pos);
}

template<typename T>
inline const T* ListConstIterator<T>::operator->() const
{
  return m_list->ElementAt(m_pos);
}

template<typename T>
inline ListConstIterator<T>& ListConstIterator<T>::operator++()
{
  ++m_pos;
  return *this;
}

template<typename T>
inline ListConstIterator<T> ListConstIterator<T>::operator++(int)
{
  ListConstIterator temp = *this;
  ++*this;
  return temp;
}

template<typename T>
inline ListConstIterator<T>& ListConstIterator<T>::operator--()
{
  --m_pos;
  return *this;
}

template<typename T>
inline ListConstIterator<T> ListConstIterator<T>::operator--(int)
{
  ListConstIterator temp = *this;
  --*this;
  return temp;
}

template<typename T>
inline bool ListConstIterator<T>::operator<(const ListConstIterator& other) const
{
  return m_list == other.m_list && m_pos < other.m_pos;
}

template<typename T>
inline bool ListConstIterator<T>::operator<=(const ListConstIterator& other) const
{
  return m_list == other.m_list && m_pos <= other.m_pos;
}

template<typename T>
inline bool ListConstIterator<T>::operator>(const ListConstIterator& other) const
{
  return m_list == other.m_list && m_pos > other.m_pos;
}

template<typename T>
inline bool ListConstIterator<T>::operator>=(const ListConstIterator& other) const
{
  return m_list == other.m_list && m_pos >= other.m_pos;
}

template<typename T>
inline ListConstIterator<T>& ListConstIterator<T>::operator+=(ptrdiff_t i)
{
  m_pos += i;
  return *this;
}

template<typename T>
inline ListConstIterator<T> ListConstIterator<T>::operator+(ptrdiff_t i) const
{
  return ListConstIterator(m_list, m_pos + i);
}

template<typename T>
inline ListConstIterator<T>& ListConstIterator<T>::operator-=(ptrdiff_t i)
{
  m_pos -= i;
  return *this;
}

template<typename T>
inline ListConstIterator<T> ListConstIterator<T>::operator-(ptrdiff_t i) const
{
  return ListConstIterator(m_list, m_pos - i);
}

template<typename T>
inline ptrdiff_t ListConstIterator<T>::operator-(const ListConstIterator& other) const
{
  return m_pos - other.m_pos;
}

template<typename T>
inline const T& ListConstIterator<T>::operator[](ptrdiff_t i) const
{
  return *m_list->ElementAt(m_pos + i);
}

template<typename T>
ListConstIterator<T> operator+(ptrdiff_t i, const ListConstIterator<T>& itr)
{
  return itr + i;
}

inline IntListPtr CreateIntList()
{
  return IntListPtr(_CreateIntList());
}

inline UIntListPtr CreateUIntList()
{
  return UIntListPtr(_CreateUIntList());
}

inline StringListPtr CreateStringList()
{
  return StringListPtr(_CreateStringList());
}

inline BoolListPtr CreateBoolList()
{
  return BoolListPtr(_CreateBoolList());
}

inline ActorDataListPtr CreateActorDataList()
{
  return ActorDataListPtr(_CreateActorDataList());
}

inline ActorSlotListPtr CreateActorSlotList()
{
  return ActorSlotListPtr(_CreateActorSlotList());
}

inline CameraPropertyValueListPtr CreateCameraPropertyValueList()
{
  return CameraPropertyValueListPtr(_CreateCameraPropertyValueList());
}

inline NotificationListPtr CreateNotificationList()
{
  return NotificationListPtr(_CreateNotificationList());
}

inline DriverInfoListPtr CreateDriverInfoList()
{
  return DriverInfoListPtr(_CreateDriverInfoList());
}

inline InertialSensorDataListPtr CreateInertialSensorDataList()
{
  return InertialSensorDataListPtr(_CreateInertialSensorDataList());
}

} // end ns sdk2
} // end ns om
