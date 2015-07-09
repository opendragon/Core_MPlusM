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
inline
OMPtr<T>::OMPtr()
: m_object(NULL)
{}

template<typename T>
inline
OMPtr<T>::OMPtr(T* object)
: m_object(object)
{}

template<typename T>
inline
OMPtr<T>::OMPtr(const OMPtr& other)
: m_object(other.m_object)
{
  if (NULL != m_object)
  {
    m_object->AddRef();
  }
}

template<typename T>
template<typename U>
inline
OMPtr<T>::OMPtr(const OMPtr<U>& other)
: m_object(other.GetPtr())
{
  if (NULL != m_object)
  {
    m_object->AddRef();
  }
}

#if defined(_MSC_VER) && _MSC_VER >= 1600
template<typename T>
inline
OMPtr<T>::OMPtr(OMPtr&& other)
: m_object(other.m_object)
{
  other.m_object = NULL;
}

template<typename T>
template<typename U>
inline
OMPtr<T>::OMPtr(OMPtr<U>&& other)
: m_object(other.m_object)
{
  other.m_object = NULL;
}
#endif

template<typename T>
inline
OMPtr<T>::~OMPtr()
{
  if (NULL != m_object)
  {
    m_object->Release();
  }
}

template<typename T>
inline
OMPtr<T>& OMPtr<T>::operator=(const OMPtr& other)
{
  if (this == &other)
  {
    return *this;
  }
  if (NULL != m_object)
  {
    m_object->Release();
  }
  m_object = other.m_object;
  if (NULL != m_object)
  {
    m_object->AddRef();
  }
  return *this;
}

template<typename T>
template<typename U>
inline
OMPtr<T>& OMPtr<T>::operator=(const OMPtr<U>& other)
{
  if (NULL != m_object)
  {
    m_object->Release();
  }
  m_object = other.GetPtr();
  if (NULL != m_object)
  {
    m_object->AddRef();
  }
  return *this;
}

template<typename T>
inline
OMPtr<T>& OMPtr<T>::operator=(T* object)
{
  if (NULL != m_object)
  {
    m_object->Release();
  }
  m_object = object;
  return *this;
}

template<typename T>
inline
T* OMPtr<T>::operator->() const
{
  return m_object;
}

template<typename T>
inline
T& OMPtr<T>::operator*() const
{
  return *m_object;
}

template<typename T>
inline
bool OMPtr<T>::IsValid() const
{
  return (NULL != m_object);
}

template<typename T>
inline
T* OMPtr<T>::GetPtr() const
{
  return m_object;
}

template<typename T>
inline
void OMPtr<T>::Clear()
{
  if (NULL != m_object)
  {
    m_object->Release();
    m_object = NULL;
  }
}

template<typename T>
template<typename U>
inline
OMPtr<U> OMPtr<T>::Cast() const
{
  if (NULL != m_object)
  {
    m_object->AddRef();
  }
  return OMPtr<U>(static_cast<U*>(m_object));
}

template<typename T>
template<typename U>
inline
bool OMPtr<T>::operator==(const OMPtr<U>& rhs) const
{
  return m_object == rhs.GetPtr();
}

template<typename T>
template<typename U>
inline
bool OMPtr<T>::operator!=(const OMPtr<U>& rhs) const
{
  return m_object != rhs.GetPtr();
}

} // end ns sdk2
} // end ns om
