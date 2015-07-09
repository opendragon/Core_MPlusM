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
SafeRef<T>::SafeRef(T& x)
: m_x(x)
{}

template<typename T>
SafeRef<T>& SafeRef<T>::operator=(const T& x)
{
  m_x = x;
  return *this;
}

template<typename T>
SafeRef<T>::operator T&()
{
  return m_x;
}

template<typename T>
SafeRef<T>::operator const T&() const
{
  return m_x;
}

} // end ns sdk2
} // end ns om
