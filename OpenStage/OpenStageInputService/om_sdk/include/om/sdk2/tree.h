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
#ifndef OM_SDK2_TREE_H
#define OM_SDK2_TREE_H

#include "om/sdk2/object.h"
#include "om/sdk2/string.h"
#include "om/sdk2/safe_auto_ptr.h"
#include "om/sdk2/declspec.h"
#include <iterator>

namespace om
{
namespace sdk2
{

struct Joint;
struct Segment;

#ifndef DOXYGEN_INVOKED
/// <summary>An internal iterator for a tree with string keys stored by the SDK,
///   used by the SDK implementation.
/// </summary>
template<typename T>
class ITreeInternalIterator : public IOMObject
{
public:
  /// <summary>Smart pointer to an iterator of the same type as this iterator.</summary>
  typedef OMPtr<ITreeInternalIterator<T> > IteratorPtr;

  /// <summary>Advance this iterator to the next node in the tree.</summary>
  ///
  /// <returns>True if it succeeds or false if the end has been reached.</returns>
  virtual bool OMCALL Next() = 0;

  /// <summary>Get the parent of this item.</summary>
  ///
  /// <returns>An iterator pointing to the parent.</returns>
  IteratorPtr GetParent() const;

  /// <summary>Get the left child of this item.</summary>
  ///
  /// <returns>An iterator pointing to the left child.</returns>
  IteratorPtr GetLeftChild() const;

  /// <summary>Get the next sibling of this item.</summary>
  ///
  /// <returns>An iterator pointing to the next sibling.</returns>
  IteratorPtr GetSibling() const;

  /// <summary>Add a new child to the node referenced by this iterator.</summary>
  ///
  /// <param name="key">The key for the new node.</param>
  ///
  /// <returns>An iterator pointing to the newly inserted node.</returns>
  IteratorPtr InsertChild(const char* key);

  /// <summary>Add a new child to the node referenced by this iterator.</summary>
  ///
  /// <param name="key">The key for the new node.</param>
  /// <param name="data">The data for the new node.</param>
  ///
  /// <returns>An iterator pointing to the newly inserted node.</returns>
  IteratorPtr InsertChildWithData(const char* key, const T& data);

  /// <summary>Get the key.</summary>
  ///
  /// <returns>The key.</returns>
  StringPtr GetKey() const;

  /// <summary>Get a pointer to the data stored in the node.</summary>
  ///
  /// <returns>The data pointer.</returns>
  virtual T* OMCALL GetData() const = 0;

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
  virtual ITreeInternalIterator<T>* OMCALL _GetParent() const = 0;
  virtual ITreeInternalIterator<T>* OMCALL _GetLeftChild() const = 0;
  virtual ITreeInternalIterator<T>* OMCALL _InsertChild(const char* key) = 0;
  virtual ITreeInternalIterator<T>* OMCALL _InsertChildWithData(const char* key, const T& data) = 0;
  virtual IString* OMCALL _GetKey() const = 0;
  virtual ITreeInternalIterator<T>* OMCALL _GetSibling() const = 0;
  virtual ITreeInternalIterator<T>* OMCALL _Clone() const = 0;
};
#endif

template<typename T>
class TreeIterator;

template<typename T>
class TreeConstIterator;

/// <summary>A tree with string keys stored by the SDK.</summary>
template<typename T>
class ITree : public IOMObject
{
public:
  /// <summary>Smart pointer to a tree of the same type as this tree.</summary>
  typedef OMPtr<ITree<T> > TreePtr;

  /// <summary>Constant smart pointer to a tree of the same type as this tree.</summary>
  typedef OMPtr<const ITree<T> > TreeConstPtr;

  /// <summary>Get an iterator referencing the first item in the tree.</summary>
  ///
  /// <returns>An iterator referencing the first item in the tree.</returns>
  /**@{*/
  TreeIterator<T> Begin();
  TreeConstIterator<T> Begin() const;
  /**@}*/

  /// <summary>Get an iterator used to signal the end of iteration.</summary>
  ///
  /// <returns>An iterator used to signal the end of iteration.</returns>
  /**@{*/
  TreeIterator<T> End();
  TreeConstIterator<T> End() const;
  /**@}*/

  /// <summary>Get a constant iterator referencing the first item in the tree.</summary>
  ///
  /// <returns>A constant iterator referencing the first item in the tree.</returns>
  TreeConstIterator<T> CBegin() const;

  /// <summary>Get a constant iterator used to signal the end of iteration.</summary>
  ///
  /// <returns>A constant iterator used to signal the end of iteration.</summary>
  TreeConstIterator<T> CEnd() const;

  /// <summary>Find a node with the specified key.</summary>
  ///
  /// <param name="key">The key to search for.</param>
  ///
  /// <returns>An iterator referencing the node found, or else a null pointer.</returns>
  /**@{*/
  TreeIterator<T> Find(const char* key);
  TreeConstIterator<T> Find(const char* key) const;
  /**@}*/

  /// <summary>Check whether the hierarchy of this tree matches that of another
  ///   tree.
  /// </summary>
  ///
  /// <param name="other">Another tree.</param>
  ///
  /// <remarks><para>Two trees are considered to have equal hierarchies if they
  ///   have the same nodes with the same keys arranged in the same structure,
  ///   but the data may be different.
  /// </para></remarks>
  ///
  /// <returns>True if the hierarchies are equal or false if they are not.</returns>
  virtual bool OMCALL IsEqualHierarchy(const TreeConstPtr& other) const = 0;

  /// <summary>Copy the contents of another tree into this one.</summary>
  ///
  /// <param name="other">Another tree.</param>
  ///
  /// <remarks><para>This function will remove all nodes currently in the tree.</para></remarks>
  virtual void OMCALL Copy(const TreeConstPtr& other) = 0;

  /// <summary>Copy the hierarchy of another tree into this one.</summary>
  ///
  /// <param name="other">Another tree.</param>
  ///
  /// <remarks><para>This function will remove all nodes currently in the tree.
  ///   Copying the hierarchy will copy the nodes and their keys but set all
  ///   data values to empty.
  /// </remarks></para>
  virtual void OMCALL CopyHierarchy(const TreeConstPtr& other) = 0;
  
  /// <summary>Clear this object to its blank/initial state.</summary>
  virtual void OMCALL Clear() = 0;

  /// <summary>Get the number of nodes in the tree.</summary>
  ///
  /// <returns>The number of nodes in the tree.</returns>
  virtual size_t OMCALL GetSize() const = 0;

  /// <summary>Insert a new root node.</summary>
  ///
  /// <param name="key">The key for the new root node.</param>
  ///
  /// <returns>An iterator pointing to the newly inserted node.</returns>
  TreeIterator<T> InsertRoot(const char* key);

  /// <summary>Insert a new root node.</summary>
  ///
  /// <param name="key">The key for the new root node.</param>
  /// <param name="data">The data for the new root node.</param>
  ///
  /// <returns>An iterator pointing to the newly inserted node.</returns>
  TreeIterator<T> InsertRootWithData(const char* key, const T& data);

  /// <summary>Add a new child to the specified node.</summary>
  ///
  /// <param name="parent">An iterator specifying the parent of the new node.</param>
  /// <param name="key">The key for the new node.</param>
  ///
  /// <returns>An iterator pointing to the newly inserted node.</returns>
  TreeIterator<T> InsertChild(const TreeIterator<T>& parent, const char* key);

  /// <summary>Add a new child to the specified node.</summary>
  ///
  /// <param name="parent">An iterator specifying the parent of the new node.</param>
  /// <param name="key">The key for the new node.</param>
  /// <param name="data">The data for the new node.</param>
  ///
  /// <returns>An iterator pointing to the newly inserted node.</returns>
  TreeIterator<T> InsertChildWithData(const TreeIterator<T>& parent,
                                      const char* key, const T& data);

  /// <summary>Get the parent of the specified item.</summary>
  ///
  /// <param name="itr">An iterator.</param>
  ///
  /// <returns>An iterator pointing to the parent of the specified item.</returns>
  /**@{*/
  TreeIterator<T> GetParent(const TreeIterator<T>& itr);
  TreeConstIterator<T> GetParent(const TreeConstIterator<T>& itr) const;
  /**@}*/

  /// <summary>Get the left child of the specified item.</summary>
  ///
  /// <param name="itr">An iterator.</param>
  ///
  /// <returns>An iterator pointing to the left child of the specified item.</returns>
  /**@{*/
  TreeIterator<T> GetLeftChild(const TreeIterator<T>& itr);
  TreeConstIterator<T> GetLeftChild(const TreeConstIterator<T>& itr) const;
  /**@}*/

  /// <summary>Get the next sibling of the specified item.</summary>
  ///
  /// <param name="itr">An iterator.</param>
  ///
  /// <returns>An iterator pointing to the next sibling of the specified item.</returns>
  /**@{*/
  TreeIterator<T> GetSibling(const TreeIterator<T>& itr);
  TreeConstIterator<T> GetSibling(const TreeConstIterator<T>& itr) const;
  /**@}*/

#ifndef DOXYGEN_INVOKED
protected:
  virtual ITreeInternalIterator<T>* OMCALL _Begin() = 0;
  virtual ITreeInternalIterator<T>* OMCALL _Find(const char* key) = 0;
  virtual ITreeInternalIterator<T>* OMCALL _InsertRoot(const char* key) = 0;
  virtual ITreeInternalIterator<T>* OMCALL _InsertRootWithData(const char* key,
                                                               const T& data) = 0;

private:
  typedef OMPtr<ITreeInternalIterator<T> > IteratorPtr;
#endif
};

/// <summary>An iterator for a tree with string keys.</summary>
template<typename T>
class TreeIterator
: public std::iterator<std::forward_iterator_tag, std::pair<const StringConstPtr, T* const> >
{
public:
#ifndef DOXYGEN_INVOKED
  /// <summary>Smart pointer to an internal iterator of the same type as this
  ///   iterator.
  /// </summary>
  typedef OMPtr<ITreeInternalIterator<T> > IteratorPtr;
#endif

  /// <summary>Key/value pair referenced by an iterator.</summary>
  typedef std::pair<const StringConstPtr, T* const> KeyValuePair;

  TreeIterator();

#ifndef DOXYGEN_INVOKED
  /// <summary>Construct an iterator from an internal iterator.</summary>
  ///
  /// <param name="it">The internal iterator.</param>
  TreeIterator(const IteratorPtr& it);
#endif

  /// <summary>Construct an iterator from another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  TreeIterator(const TreeIterator& other);

  /// <summary>Set this iterator to be a copy of another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const TreeIterator& operator=(const TreeIterator& other);

  /// <summary>Check if this iterator is equal to another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>True if the iterators are equal or false otherwise.</returns>
  bool operator==(const TreeIterator& other) const;

  /// <summary>Check if this iterator is unequal to another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>True if the iterators are unequal or false otherwise.</returns>
  bool operator!=(const TreeIterator& other) const;

  /// <summary>Access the key/value pair referenced by this iterator.</summary>
  ///
  /// <returns>Reference to the key/value pair referenced by this iterator.</returns>
  KeyValuePair& operator*() const;

  /// <summary>Access the key/value pair referenced by this iterator.</summary>
  ///
  /// <returns>Pointer to the key/value pair referenced by this iterator.</returns>
  KeyValuePair* operator->() const;

  /// <summary>Advance to the next item in the tree.</summary>
  ///
  /// <returns>Reference to this iterator.</returns>
  TreeIterator& operator++();

  /// <summary>Advance to the next item in the tree.</summary>
  ///
  /// <returns>A copy of this iterator before advancing.</returns>
  TreeIterator operator++(int);

private:
  IteratorPtr m_it;
  SafeAutoPtr<KeyValuePair> m_kvp;

  friend class ITree<T>;
  friend class TreeConstIterator<T>;
};

/// <summary>A constant iterator for a tree with string keys.</summary>
template<typename T>
class TreeConstIterator
: public std::iterator<std::forward_iterator_tag,
                       std::pair<const StringConstPtr, const T* const> >
{
public:
#ifndef DOXYGEN_INVOKED
  /// <summary>Smart pointer to an internal iterator of the same type as this
  ///   constant iterator.
  /// </summary>
  typedef OMPtr<ITreeInternalIterator<T> > IteratorPtr;
#endif

  /// <summary>Key/value pair referenced by a constant iterator.</summary>
  typedef std::pair<const StringConstPtr, const T* const> KeyValuePair;

  TreeConstIterator();

#ifndef DOXYGEN_INVOKED
  /// <summary>Construct a constant iterator from an internal iterator.</summary>
  ///
  /// <param name="it">The internal iterator.</param>
  TreeConstIterator(const IteratorPtr& it);
#endif

  /// <summary>Construct a constant iterator from another constant iterator.</summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  TreeConstIterator(const TreeConstIterator& other);

  /// <summary>Construct a constant iterator from an iterator.</summary>
  ///
  /// <param name="other">The iterator.</param>
  TreeConstIterator(const TreeIterator<T>& other);

  /// <summary>Set this constant iterator to be a copy of another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const TreeConstIterator& operator=(const TreeConstIterator& other);

  /// <summary>Set this constant iterator to be a copy of an iterator.</summary>
  ///
  /// <param name="other">The iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const TreeConstIterator& operator=(const TreeIterator<T>& other);

  /// <summary>Check if this constant iterator is equal to another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>True if the constant iterators are equal or false
  ///   otherwise.
  /// </returns>
  bool operator==(const TreeConstIterator& other) const;

  /// <summary>Check if this constant iterator is unequal to another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>True if the constant iterators are unequal or false
  ///   otherwise.
  /// </returns>
  bool operator!=(const TreeConstIterator& other) const;

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

  /// <summary>Advance to the next item in the tree.</summary>
  ///
  /// <returns>Reference to this constant iterator.</returns>
  TreeConstIterator& operator++();

  /// <summary>Advance to the next item in the tree.</summary>
  ///
  /// <returns>A copy of this constant iterator before advancing.</returns>
  TreeConstIterator operator++(int);

private:
  IteratorPtr m_it;
  SafeAutoPtr<KeyValuePair> m_kvp;

  friend class ITree<T>;
};

/// <summary>Smart pointer reference to a skeleton.</summary>
typedef OMPtr<ITree<Segment> > SkeletonPtr;

/// <summary>Constant smart pointer reference to a skeleton.</summary>
typedef OMPtr<const ITree<Segment> > SkeletonConstPtr;

/// <summary>Iterator for a skeleton.</summary>
typedef TreeIterator<Segment> SkeletonIterator;

/// <summary>Constant iterator for a skeleton.</summary>
typedef TreeConstIterator<Segment> SkeletonConstIterator;

/// <summary>Smart pointer reference to a joint tree.</summary>
typedef OMPtr<ITree<Joint> > JointTreePtr;

/// <summary>Constant smart pointer reference to a joint tree.</summary>
typedef OMPtr<const ITree<Joint> > JointTreeConstPtr;

/// <summary>Iterator for a joint tree.</summary>
typedef TreeIterator<Joint> JointTreeIterator;

/// <summary>Constant iterator for a joint tree.</summary>
typedef TreeConstIterator<Joint> JointTreeConstIterator;

/// <summary>Creates a skeleton.</summary>
///
/// <returns>An empty skeleton.</returns>
SkeletonPtr CreateSkeleton();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ITree<Segment>* OMCALL _CreateSkeleton();
#endif

/// <summary>Creates a joint tree.</summary>
///
/// <returns>An empty joint tree.</returns>
JointTreePtr CreateJointTree();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ITree<Joint>* OMCALL _CreateJointTree();
#endif

} // end ns sdk2
} // end ns om

#include "om/sdk2/tree.inl"

#endif //OM_SDK2_TREE_H
