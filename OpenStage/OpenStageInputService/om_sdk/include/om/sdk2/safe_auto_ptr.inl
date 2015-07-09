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
inline SafeAutoPtr<T>::SafeAutoPtr()
: m_p(NULL)
{}

template<typename T>
inline SafeAutoPtr<T>::~SafeAutoPtr()
{
  if (NULL != m_p)
  {
    delete m_p;
  }
}

template<typename T>
inline SafeAutoPtr<T>::SafeAutoPtr(T* p)
: m_p(p)
{}

template<typename T>
inline SafeAutoPtr<T>& SafeAutoPtr<T>::operator=(T* p)
{
  if (NULL != m_p)
  {
    delete m_p;
  }
  m_p = p;
  return *this;
}

template<typename T>
inline T* SafeAutoPtr<T>::operator->() const
{
  return m_p;
}

template<typename T>
inline T& SafeAutoPtr<T>::operator*() const
{
  return *m_p;
}

template<typename T>
inline bool SafeAutoPtr<T>::IsValid() const
{
  return NULL != m_p;
}

template<typename T>
inline T* SafeAutoPtr<T>::GetPtr() const
{
  return m_p;
}

} // end ns sdk2
} // end ns om
