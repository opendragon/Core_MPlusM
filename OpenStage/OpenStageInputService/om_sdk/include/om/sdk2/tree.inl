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
template<typename T>
inline OMPtr<ITreeInternalIterator<T> > ITreeInternalIterator<T>::GetParent() const
{
  return IteratorPtr(_GetParent());
}

template<typename T>
inline OMPtr<ITreeInternalIterator<T> > ITreeInternalIterator<T>::GetLeftChild() const
{
  return IteratorPtr(_GetLeftChild());
}

template<typename T>
inline OMPtr<ITreeInternalIterator<T> > ITreeInternalIterator<T>::GetSibling() const
{
  return IteratorPtr(_GetSibling());
}

template<typename T>
inline OMPtr<ITreeInternalIterator<T> > ITreeInternalIterator<T>::InsertChild(const char* key)
{
  return IteratorPtr(_InsertChild(key));
}

template<typename T>
inline OMPtr<ITreeInternalIterator<T> > ITreeInternalIterator<T>::InsertChildWithData(
  const char* key, const T& data)
{
  return IteratorPtr(_InsertChildWithData(key, data));
}

template<typename T>
inline StringPtr ITreeInternalIterator<T>::GetKey() const
{
  return StringPtr(_GetKey());
}

template<typename T>
inline OMPtr<ITreeInternalIterator<T> > ITreeInternalIterator<T>::Clone() const
{
  return IteratorPtr(_Clone());
}
#endif //DOXYGEN_INVOKED

template<typename T>
inline TreeIterator<T> ITree<T>::Begin()
{
  return IteratorPtr(_Begin());
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::Begin() const
{
  return IteratorPtr(const_cast<ITree<T>*>(this)->_Begin());
}

template<typename T>
inline TreeIterator<T> ITree<T>::End()
{
  return TreeIterator<T>();
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::End() const
{
  return TreeConstIterator<T>();
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::CBegin() const
{
  return IteratorPtr(const_cast<ITree<T>*>(this)->_Begin());
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::CEnd() const
{
  return TreeConstIterator<T>();
}

template<typename T>
inline TreeIterator<T> ITree<T>::Find(const char* key)
{
  return IteratorPtr(_Find(key));
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::Find(const char* key) const
{
  return IteratorPtr(const_cast<ITree<T>*>(this)->_Find(key));
}

template<typename T>
inline TreeIterator<T> ITree<T>::InsertRoot(const char* key)
{
  return IteratorPtr(_InsertRoot(key));
}

template<typename T>
inline TreeIterator<T> ITree<T>::InsertRootWithData(
  const char* key, const T& data)
{
  return IteratorPtr(_InsertRootWithData(key, data));
}

template<typename T>
inline TreeIterator<T> ITree<T>::InsertChild(const TreeIterator<T>& parent,
                                             const char* key)
{
  if (!parent.m_it.IsValid())
  {
    return TreeIterator<T>();
  }
  return parent.m_it->InsertChild(key);
}

template<typename T>
inline TreeIterator<T> ITree<T>::InsertChildWithData(const TreeIterator<T>& parent,
                                                     const char* key,
                                                     const T& data)
{
  if (!parent.m_it.IsValid())
  {
    return TreeIterator<T>();
  }
  return parent.m_it->InsertChildWithData(key, data);
}

template<typename T>
inline TreeIterator<T> ITree<T>::GetParent(const TreeIterator<T>& itr)
{
  if (!itr.m_it.IsValid())
  {
    return TreeIterator<T>();
  }
  return itr.m_it->GetParent();
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::GetParent(const TreeConstIterator<T>& itr) const
{
  if (!itr.m_it.IsValid())
  {
    return TreeConstIterator<T>();
  }
  return itr.m_it->GetParent();
}

template<typename T>
inline TreeIterator<T> ITree<T>::GetLeftChild(const TreeIterator<T>& itr)
{
  if (!itr.m_it.IsValid())
  {
    return TreeIterator<T>();
  }
  return itr.m_it->GetLeftChild();
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::GetLeftChild(const TreeConstIterator<T>& itr) const
{
  if (!itr.m_it.IsValid())
  {
    return TreeConstIterator<T>();
  }
  return itr.m_it->GetLeftChild();
}

template<typename T>
inline TreeIterator<T> ITree<T>::GetSibling(const TreeIterator<T>& itr)
{
  if (!itr.m_it.IsValid())
  {
    return TreeIterator<T>();
  }
  return itr.m_it->GetSibling();
}

template<typename T>
inline TreeConstIterator<T> ITree<T>::GetSibling(const TreeConstIterator<T>& itr) const
{
  if (!itr.m_it.IsValid())
  {
    return TreeConstIterator<T>();
  }
  return itr.m_it->GetSibling();
}

template<typename T>
inline TreeIterator<T>::TreeIterator()
: m_it(),
  m_kvp()
{}

#ifndef DOXYGEN_INVOKED
template<typename T>
inline TreeIterator<T>::TreeIterator(const IteratorPtr& it)
: m_it(it),
  m_kvp()
{
  if (m_it.IsValid())
  {
    m_kvp = new KeyValuePair(m_it->GetKey(), m_it->GetData());
  }
}
#endif

template<typename T>
inline TreeIterator<T>::TreeIterator(const TreeIterator& other)
: m_it(),
  m_kvp()
{
  *this = other;
}

template<typename T>
inline const TreeIterator<T>& TreeIterator<T>::operator=(
  const TreeIterator& other)
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

template<typename T>
inline bool TreeIterator<T>::operator==(const TreeIterator& other) const
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

template<typename T>
inline bool TreeIterator<T>::operator!=(const TreeIterator& other) const
{
  return !(*this == other);
}

template<typename T>
inline typename TreeIterator<T>::KeyValuePair& TreeIterator<T>::operator*() const
{
  return *m_kvp;
}

template<typename T>
inline typename TreeIterator<T>::KeyValuePair* TreeIterator<T>::operator->() const
{
  return m_kvp.GetPtr();
}

template<typename T>
inline TreeIterator<T>& TreeIterator<T>::operator++()
{
  if (m_kvp.IsValid() && m_it.IsValid())
  {
    if (m_it->Next())
    {
      m_kvp = new KeyValuePair(m_it->GetKey(), m_it->GetData());
    }
    else
    {
      m_kvp = NULL;
    }
  }
  return *this;
}

template<typename T>
inline TreeIterator<T> TreeIterator<T>::operator++(int)
{
  TreeIterator temp = *this;
  ++*this;
  return temp;
}

template<typename T>
inline TreeConstIterator<T>::TreeConstIterator()
: m_it(),
  m_kvp()
{}

#ifndef DOXYGEN_INVOKED
template<typename T>
inline TreeConstIterator<T>::TreeConstIterator(const IteratorPtr& it)
: m_it(it),
  m_kvp()
{
  if (m_it.IsValid())
  {
    m_kvp = new KeyValuePair(m_it->GetKey(), m_it->GetData());
  }
}
#endif

template<typename T>
inline TreeConstIterator<T>::TreeConstIterator(const TreeConstIterator& other)
: m_it(),
  m_kvp()
{
  *this = other;
}

template<typename T>
inline TreeConstIterator<T>::TreeConstIterator(const TreeIterator<T>& other)
: m_it(),
  m_kvp()
{
  *this = other;
}

template<typename T>
inline const TreeConstIterator<T>& TreeConstIterator<T>::operator=(
  const TreeConstIterator& other)
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

template<typename T>
inline const TreeConstIterator<T>& TreeConstIterator<T>::operator=(
  const TreeIterator<T>& other)
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

template<typename T>
inline bool TreeConstIterator<T>::operator==(const TreeConstIterator& other) const
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

template<typename T>
inline bool TreeConstIterator<T>::operator!=(const TreeConstIterator& other) const
{
  return !(*this == other);
}

template<typename T>
inline typename TreeConstIterator<T>::KeyValuePair& TreeConstIterator<T>::operator*() const
{
  return *m_kvp;
}

template<typename T>
inline typename TreeConstIterator<T>::KeyValuePair* TreeConstIterator<T>::operator->() const
{
  return m_kvp.GetPtr();
}

template<typename T>
inline TreeConstIterator<T>& TreeConstIterator<T>::operator++()
{
  if (m_kvp.IsValid() && m_it.IsValid())
  {
    if (m_it->Next())
    {
      m_kvp = new KeyValuePair(m_it->GetKey(), m_it->GetData());
    }
    else
    {
      m_kvp = NULL;
    }
  }
  return *this;
}

template<typename T>
inline TreeConstIterator<T> TreeConstIterator<T>::operator++(int)
{
  TreeConstIterator temp = *this;
  ++*this;
  return temp;
}

inline SkeletonPtr CreateSkeleton()
{
  return SkeletonPtr(_CreateSkeleton());
}

inline JointTreePtr CreateJointTree()
{
  return JointTreePtr(_CreateJointTree());
}

} // end ns sdk2
} // end ns om
