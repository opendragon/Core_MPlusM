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
#ifndef OM_SDK2_LIST_H
#define OM_SDK2_LIST_H

#include "om/sdk2/object.h"
#include "om/sdk2/declspec.h"
#include "om/sdk2/string.h"
#include <iterator>

namespace om
{
namespace sdk2
{

struct ActorData;
struct ActorSlot;
struct CameraPropertyValue;
struct DriverInfo;
struct InertialSensorData;
struct Notification;

template<typename T>
class ListIterator;

template<typename T>
class ListConstIterator;

/// <summary>A list stored by the SDK.</summary>
template<typename T>
class IList : public IOMObject
{
public:
  /// <summary>Get the length of the list.</summary>
  ///
  /// <returns>The length of the list.</returns>
  virtual size_t OMCALL GetSize() const = 0;

  /// <summary>Set the length of the list.</summary>
  ///
  /// <param name="size">The new size of the list.</param>
  virtual void OMCALL SetSize(size_t size) = 0;

  /// <summary>Query if this list is empty.</summary>
  ///
  /// <returns>True if the list is empty or false if it is not.</returns>
  virtual bool OMCALL IsEmpty() const = 0;

  /// <summary>Delete all items from the list.</summary>
  virtual void OMCALL Clear() = 0;

  /// <summary>Reserve enough space in memory for a given number of items.</summary>
  ///
  /// <param name="size">The number of items for which to reserve space.</param>
  virtual void OMCALL Reserve(size_t size) = 0;

  /// <summary>Add an item to the end of the list.</summary>
  ///
  /// <param name="item">The item to add.</param>
  virtual void OMCALL Add(const T& item) = 0;

  /// <summary>Set the item at a given index.</summary>
  ///
  /// <param name="i">Zero-based index of the item.</param>
  /// <param name="item">The new value of the item.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL SetAt(size_t i, const T& item) = 0;

  /// <summary>Get the item at a given index.</summary>
  ///
  /// <param name="i">Zero-based index of the item.</param>
  /// <param name="item">Pointer to receive the item.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL GetAt(size_t i, T* item) const = 0;

  /// <summary>Get a pointer to the item at a given index.</summary>
  ///
  /// <param name="i">Zero-based index of the item.</param>
  ///
  /// <remarks><para>This function is unsupported for Boolean lists and will
  ///   always return a null pointer if called on a Boolean list.
  /// </para></remarks>
  ///
  /// <returns>A pointer to the item at the specified index or null if the index
  ///   is out of range.
  /// </returns>
  /**@{*/
  virtual const T* OMCALL ElementAt(size_t i) const = 0;
  T* ElementAt(size_t i);
  /**@}*/

  /// <summary>Get the item at a given index.</summary>
  ///
  /// <param name="i">Zero-based index of the item.</param>
  ///
  /// <returns>The item.</returns>
  T GetAt(size_t i) const;

  /// <summary>Remove the item at a given index.</summary>
  ///
  /// <param name="i">Zero-based index of the item.</param>
  ///
  /// <returns>True if it succeeds or false if it fails.</returns>
  virtual bool OMCALL RemoveAt(size_t i) = 0;

  /// <summary>Copy the contents of another list.</summary>
  ///
  /// <param name="list">The list to copy.</param>
  virtual void OMCALL CopyList(const OMPtr<const IList>& list) = 0;

  /// <summary>Get an iterator referencing the first item in the list.</summary>
  ///
  /// <returns>An iterator referencing the first item in the list.</returns>
  /**@{*/
  ListIterator<T> Begin();
  ListConstIterator<T> Begin() const;
  /**@}*/

  /// <summary>Get an iterator used to signal the end of iteration.</summary>
  ///
  /// <returns>An iterator used to signal the end of iteration.</returns>
  /**@{*/
  ListIterator<T> End();
  ListConstIterator<T> End() const;
  /**@}*/

  /// <summary>Get a constant iterator referencing the first item in the
  ///   list.
  /// </summary>
  ///
  /// <returns>A constant iterator referencing the first item in the
  ///   list.
  /// </returns>
  ListConstIterator<T> CBegin() const;

  /// <summary>Get a constant iterator used to signal the end of
  ///   iteration.
  /// </summary>
  ///
  /// <returns>A constant iterator used to signal the end of
  ///   iteration.
  /// </returns>
  ListConstIterator<T> CEnd() const;

  /// <summary>Get a reverse iterator referencing the last item in the
  ///   list.
  /// </summary>
  ///
  /// <returns>A reverse iterator referencing the last item in the
  ///   list.
  /// </returns>
  /**@{*/
  std::reverse_iterator<ListIterator<T> > RBegin();
  std::reverse_iterator<ListConstIterator<T> > RBegin() const;
  /**@}*/

  /// <summary>Get a reverse iterator used to signal the end of
  ///   iteration.
  /// </summary>
  ///
  /// <returns>A reverse iterator used to signal the end of iteration.</returns>
  /**@{*/
  std::reverse_iterator<ListIterator<T> > REnd();
  std::reverse_iterator<ListConstIterator<T> > REnd() const;
  /**@}*/

  /// <summary>Get a constant reverse iterator referencing the last item in the
  ///   list.
  /// </summary>
  ///
  /// <returns>A constant reverse iterator referencing the last item in the
  ///   list.
  /// </returns>
  std::reverse_iterator<ListConstIterator<T> > CRBegin() const;

  /// <summary>Get a constant reverse iterator used to signal the end of
  ///   iteration.
  /// </summary>
  ///
  /// <returns>A constant reverse iterator used to signal the end of
  ///   iteration.
  /// </returns>
  std::reverse_iterator<ListConstIterator<T> > CREnd() const;
};

/// <summary>Interface providing extra list methods specific a particular item
/// data type.</summary>
template<typename T>
class IListExtra : public IList<T>
{
  // All methods are declared in specializations.
};

/// <summary>Interface providing extra list methods specific to string
/// lists.</summary>
template<>
class IListExtra<StringPtr> : public IList<StringPtr>
{
public:
#ifndef DOXYGEN_INVOKED
  // Redeclare this as pure virtual so that it is overloaded, rather than
  // hidden, by the function below.
  virtual void OMCALL Add(const StringPtr& item) = 0;
#endif

  /// <summary>Add a string, specified as a raw string pointer, to the end of
  /// the list.</summary>
  ///
  /// <param name="item">The string to add.</param>
  void Add(const char* item);
};

/// <summary>An iterator for a list stored by the SDK.</summary>
template<typename T>
class ListIterator : public std::iterator<std::random_access_iterator_tag, T>
{
public:
  /// <summary>Smart pointer to a list of the same type as this iterator.</summary>
  typedef OMPtr<IList<T> > ListPtr;

  ListIterator();

  /// <summary>Construct an iterator for the specified item in a list.</summary>
  ///
  /// <param name="list">The list.</param>
  /// <param name="pos">The position in the list at which to start.</param>
  ListIterator(const ListPtr& list, size_t pos);

  /// <summary>Construct an iterator from another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ListIterator(const ListIterator& other);

  /// <summary>Set this iterator to be a copy of another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const ListIterator& operator=(const ListIterator& other);

  /// <summary>Check if this iterator is equal to another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>True if the iterators are equal or false otherwise.</returns>
  bool operator==(const ListIterator& other) const;

  /// <summary>Check if this iterator is unequal to another iterator.</summary>
  ///
  /// <param name="other">The other iterator.</param>
  ///
  /// <returns>True if the iterators are unequal or false otherwise.</returns>
  bool operator!=(const ListIterator& other) const;

  /// <summary>Access the item referenced by this iterator.</summary>
  ///
  /// <returns>Reference to the item referenced by this iterator.</returns>
  T& operator*() const;

  /// <summary>Access the item referenced by this iterator.</summary>
  ///
  /// <returns>Pointer to the item referenced by this iterator.</returns>
  T* operator->() const;

  /// <summary>Advance to the next item in the list.</summary>
  ///
  /// <returns>Reference to this iterator.</returns>
  ListIterator& operator++();

  /// <summary>Advance to the next item in the list.</summary>
  ///
  /// <returns>A copy of this iterator before advancing.</returns>
  ListIterator operator++(int);

  /// <summary>Go back to the previous element in the list.</summary>
  ///
  /// <returns>Reference to this iterator.</returns>
  ListIterator& operator--();

  /// <summary>Go back to the previous element in the list.</summary>
  ///
  /// <returns>A copy of this iterator before going back.</returns>
  ListIterator operator--(int);

  /// <summary>Check if this iterator is less than another iterator.</summary>
  ///
  /// <param name="other">Another iterator.</param>
  ///
  /// <returns>True if the first iterator is less than the second.</returns>
  bool operator<(const ListIterator& other) const;

  /// <summary>Check if this iterator is less than or equal to another
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">Another iterator.</param>
  ///
  /// <returns>True if the first iterator is less then or equal to the
  ///   second.
  /// </returns>
  bool operator<=(const ListIterator& other) const;

  /// <summary>Check if this iterator is greater than another iterator.</summary>
  ///
  /// <param name="other">Another iterator.</param>
  ///
  /// <returns>True if the first iterator is greater than the second.</returns>
  bool operator>(const ListIterator& other) const;

  /// <summary>Check if this iterator is greater than or equal to another
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">Another iterator.</param>
  ///
  /// <returns>True if the first iterator is greater than or equal to the
  ///   second.
  /// </returns>
  bool operator>=(const ListIterator& other) const;

  /// <summary>Skip ahead by the specified number of items.</summary>
  ///
  /// <param name="i">The number of items to skip ahead.</param>
  ///
  /// <returns>Reference to this iterator.</returns>
  ListIterator& operator+=(ptrdiff_t i);

  /// <summary>Obtain an iterator pointing the specified number of items ahead
  ///   of this iterator.
  /// </summary>
  ///
  /// <param name="i">The number of items to skip ahead.</param>
  ///
  /// <returns>A new iterator resulting from skipping ahead the specified number
  ///   of items.
  /// </returns>
  ListIterator operator+(ptrdiff_t i) const;

  /// <summary>Go back by the specified number of items.</summary>
  ///
  /// <param name="i">The number of items to go back.</param>
  ///
  /// <returns>Reference to this iterator.</returns>
  ListIterator& operator-=(ptrdiff_t i);

  /// <summary>Obtain an iterator pointing the specified number of items behind
  ///   this iterator.
  /// </summary>
  ///
  /// <param name="i">The number of items to go back.</param>
  ///
  /// <returns>A new iterator resulting from going back the specified number of
  ///   items.
  /// </returns>
  ListIterator operator-(ptrdiff_t i) const;

  /// <summary>Obtain the offset of this iterator relative to another
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">Another iterator.</param>
  ///
  /// <returns>The offset of this iterator relative to the other
  ///   iterator.
  /// </returns>
  ptrdiff_t operator-(const ListIterator& other) const;

  /// <summary>Access the item located at a given offset relative to this
  ///   iterator.
  /// </summary>
  ///
  /// <param name="i">The offset.</param>
  ///
  /// <returns>The item at the specified offset.</returns>
  T& operator[](ptrdiff_t i) const;

private:
  ListPtr m_list;
  size_t m_pos;

  friend class ListConstIterator<T>;
};

/// <summary>Obtain an iterator pointing the specified number of items ahead
///   of the specified iterator.
/// </summary>
///
/// <param name="i">The number of items to skip ahead.</param>
/// <param name="itr">The iterator.</param>
///
/// <returns>A new iterator resulting from skipping ahead the specified number
///   of items.
/// </returns>
template<typename T>
ListIterator<T> operator+(ptrdiff_t i, const ListIterator<T>& itr);

/// <summary>A constant iterator for a list stored by the SDK.</summary>
template<typename T>
class ListConstIterator
: public std::iterator<std::random_access_iterator_tag, const T>
{
public:
  /// <summary>Constant smart pointer to a list of the same type as this
  ///   constant iterator.
  /// </summary>
  typedef OMPtr<const IList<T> > ListConstPtr;

  ListConstIterator();

  /// <summary>Construct a constant iterator for the specified item in a
  ///   list.
  /// </summary>
  ///
  /// <param name="list">The list.</param>
  /// <param name="pos">The position in the list at which to start.</param>
  ListConstIterator(const ListConstPtr& list, size_t pos);

  /// <summary>Construct a constant iterator from another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other iterator.</param>
  ListConstIterator(const ListConstIterator& other);

  /// <summary>Construct a constant iterator from an iterator.</summary>
  ///
  /// <param name="other">The iterator.</param>
  ListConstIterator(const ListIterator<T>& other);

  /// <summary>Set this constant iterator to be a copy of another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const ListConstIterator& operator=(const ListConstIterator& other);

  /// <summary>Set this constant iterator to be a copy of an iterator.</summary>
  ///
  /// <param name="other">The iterator.</param>
  ///
  /// <returns>A reference to this object.</returns>
  const ListConstIterator& operator=(const ListIterator<T>& other);

  /// <summary>Check if this constant iterator is equal to another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>True if the constant iterators are equal or false
  ///   otherwise.
  /// </returns>
  bool operator==(const ListConstIterator& other) const;

  /// <summary>Check if this constant iterator is unequal to another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">The other constant iterator.</param>
  ///
  /// <returns>True if the constant iterators are unequal or false
  ///   otherwise.
  /// </returns>
  bool operator!=(const ListConstIterator& other) const;

  /// <summary>Access the item referenced by this constant iterator.</summary>
  ///
  /// <returns>Reference to the item referenced by this constant iterator.</returns>
  const T& operator*() const;

  /// <summary>Access the item referenced by this constant iterator.</summary>
  ///
  /// <returns>Pointer to the item referenced by this constant iterator.</returns>
  const T* operator->() const;

  /// <summary>Advance to the next item in the list.</summary>
  ///
  /// <returns>Reference to this constant iterator.</returns>
  ListConstIterator& operator++();

  /// <summary>Advance to the next item in the list.</summary>
  ///
  /// <returns>A copy of this constant iterator before advancing.</returns>
  ListConstIterator operator++(int);

  /// <summary>Go back to the previous element in the list.</summary>
  ///
  /// <returns>Reference to this constant iterator.</returns>
  ListConstIterator& operator--();

  /// <summary>Go back to the previous element in the list.</summary>
  ///
  /// <returns>A copy of this constant iterator before going back.</returns>
  ListConstIterator operator--(int);

  /// <summary>Check if this constant iterator is less than another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">Another constant iterator.</param>
  ///
  /// <returns>True if the first constant iterator is less than the
  ///   second.
  /// </returns>
  bool operator<(const ListConstIterator& other) const;

  /// <summary>Check if this constant iterator is less than or equal to another
  ///   constant iterator.
  /// </summary>
  ///
  /// <param name="other">Another constant iterator.</param>
  ///
  /// <returns>True if the first constant iterator is less than or equal to the
  ///   second.
  /// </returns>
  bool operator<=(const ListConstIterator& other) const;

  /// <summary>Check if this constant iterator is greater than another constant
  ///   iterator.
  /// </summary>
  ///
  /// <param name="other">Another constant iterator.</param>
  ///
  /// <returns>True if the first constant iterator is greater than the
  ///   second.
  /// </returns>
  bool operator>(const ListConstIterator& other) const;

  /// <summary>Check if this constant iterator is greater than or equal to
  ///   another constant iterator.
  /// </summary>
  ///
  /// <param name="other">Another constant iterator.</param>
  ///
  /// <returns>True if the first constant iterator is greater than or equal to
  ///   the second.
  /// </returns>
  bool operator>=(const ListConstIterator& other) const;

  /// <summary>Skip ahead by the specified number of items.</summary>
  ///
  /// <param name="i">The number of items to skip ahead.</param>
  ///
  /// <returns>Reference to this constant iterator.</returns>
  ListConstIterator& operator+=(ptrdiff_t i);

  /// <summary>Obtain a constant iterator pointing the specified number of items
  ///   ahead of this constant iterator.
  /// </summary>
  ///
  /// <param name="i">The number of items to skip ahead.</param>
  ///
  /// <returns>A new constant iterator resulting from skipping ahead the
  ///   specified number of items.
  /// </returns>
  ListConstIterator operator+(ptrdiff_t i) const;

  /// <summary>Go back by the specified number of items.</summary>
  ///
  /// <param name="i">The number of items to go back.</param>
  ///
  /// <returns>Reference to this constant iterator.</returns>
  ListConstIterator& operator-=(ptrdiff_t i);

  /// <summary>Obtain a constant iterator pointing the specified number of items
  ///   behind this constant iterator.
  /// </summary>
  ///
  /// <param name="i">The number of items to go back.</param>
  ///
  /// <returns>A new constant iterator resulting from going back the specified
  ///   number of items.
  /// </returns>
  ListConstIterator operator-(ptrdiff_t i) const;

  /// <summary>Obtain the offset of this constant iterator relative to another
  ///   constant iterator.
  /// </summary>
  ///
  /// <param name="other">Another constant iterator.</param>
  ///
  /// <returns>The offset of this constant iterator relative to the other
  ///   constant iterator.
  /// </returns>
  ptrdiff_t operator-(const ListConstIterator& other) const;

  /// <summary>Access the item located at a given offset relative to this
  ///   constant iterator.
  /// </summary>
  ///
  /// <param name="i">The offset.</param>
  ///
  /// <returns>The item at the specified offset.</returns>
  const T& operator[](ptrdiff_t i) const;

private:
  ListConstPtr m_list;
  size_t m_pos;
};

/// <summary>Obtain a constant iterator pointing the specified number of items
///   ahead of the specified constant iterator.
/// </summary>
///
/// <param name="i">The number of items to skip ahead.</param>
/// <param name="itr">The iterator.</param>
///
/// <returns>A new constant iterator resulting from skipping ahead the specified
///   number of items.
/// </returns>
template<typename T>
ListConstIterator<T> operator+(ptrdiff_t i, const ListConstIterator<T>& itr);

/// <summary>A list of integers.</summary>
typedef IListExtra<int> IIntList;

/// <summary>Smart pointer reference to a list of integers.</summary>
typedef OMPtr<IIntList> IntListPtr;

/// <summary>Constant smart pointer reference to a list of integers.</summary>
typedef OMPtr<const IIntList> IntListConstPtr;

/// <summary>Iterator for a list of integers.</summary>
typedef ListIterator<int> IntListIterator;

/// <summary>Constant iterator for a list of integers.</summary>
typedef ListConstIterator<int> IntListConstIterator;

/// <summary>Reverse iterator for a list of integers.</summary>
typedef std::reverse_iterator<IntListIterator> IntListReverseIterator;

/// <summary>Constant reverse iterator for a list of integers.</summary>
typedef std::reverse_iterator<IntListConstIterator> IntListConstReverseIterator;

/// <summary>A list of unsigned integers.</summary>
typedef IListExtra<unsigned int> IUIntList;

/// <summary>Smart pointer reference to a list of unsigned integers.</summary>
typedef OMPtr<IUIntList> UIntListPtr;

/// <summary>Constant smart pointer reference to a list of unsigned
///   integers.
/// </summary>
typedef OMPtr<const IUIntList> UIntListConstPtr;

/// <summary>Iterator for a list of unsigned integers.</summary>
typedef ListIterator<unsigned int> UIntListIterator;

/// <summary>Constant iterator for a list of unsigned integers.</summary>
typedef ListConstIterator<unsigned int> UIntListConstIterator;

/// <summary>Reverse iterator for a list of unsigned integers.</summary>
typedef std::reverse_iterator<UIntListIterator> UIntListReverseIterator;

/// <summary>Constant reverse iterator for a list of unsigned integers.</summary>
typedef std::reverse_iterator<UIntListConstIterator>
  UIntListConstReverseIterator;

/// <summary>A list of Boolean values.</summary>
typedef IListExtra<bool> IBoolList;

/// <summary>Smart pointer reference to a list of Boolean values.</summary>
typedef OMPtr<IBoolList> BoolListPtr;

/// <summary>Constant smart pointer reference to a list of Boolean
///   values.
/// </summary>
typedef OMPtr<const IBoolList> BoolListConstPtr;

/// <summary>Iterator for a list of Boolean values.</summary>
typedef ListIterator<bool> BoolListIterator;

/// <summary>Constant iterator for a list of Boolean values.</summary>
typedef ListConstIterator<bool> BoolListConstIterator;

/// <summary>Reverse iterator for a list of Boolean values.</summary>
typedef std::reverse_iterator<BoolListIterator> BoolListReverseIterator;

/// <summary>Constant reverse iterator for a list of Boolean values.</summary>
typedef std::reverse_iterator<BoolListConstIterator>
  BoolListConstReverseIterator;

/// <summary>A list of strings.</summary>
typedef IListExtra<StringPtr> IStringList;

/// <summary>Smart pointer reference to a list of strings.</summary>
typedef OMPtr<IStringList> StringListPtr;

/// <summary>Constant smart pointer reference to a list of strings.</summary>
typedef OMPtr<const IStringList> StringListConstPtr;

/// <summary>Iterator for a list of strings.</summary>
typedef ListIterator<StringPtr> StringListIterator;

/// <summary>Constant iterator for a list of strings.</summary>
typedef ListConstIterator<StringPtr> StringListConstIterator;

/// <summary>Reverse iterator for a list of strings.</summary>
typedef std::reverse_iterator<StringListIterator>
  StringListReverseIterator;

/// <summary>Constant reverse iterator for a list of strings.</summary>
typedef std::reverse_iterator<StringListConstIterator>
  StringListConstReverseIterator;

/// <summary>A list of actor data.</summary>
typedef IListExtra<ActorData> IActorDataList;

/// <summary>Smart pointer reference to a list of actor data.</summary>
typedef OMPtr<IActorDataList> ActorDataListPtr;

/// <summary>Constant smart pointer reference to a list of actor data.</summary>
typedef OMPtr<const IActorDataList> ActorDataListConstPtr;

/// <summary>Iterator for a list of actor data.</summary>
typedef ListIterator<ActorData> ActorDataListIterator;

/// <summary>Constant iterator for a list of actor data.</summary>
typedef ListConstIterator<ActorData> ActorDataListConstIterator;

/// <summary>Reverse iterator for a list of actor data.</summary>
typedef std::reverse_iterator<ActorDataListIterator>
  ActorDataListReverseIterator;

/// <summary>Constant reverse iterator for a list of actor data.</summary>
typedef std::reverse_iterator<ActorDataListConstIterator>
  ActorDataListConstReverseIterator;

/// <summary>A list of actor slots.</summary>
typedef IListExtra<ActorSlot> IActorSlotList;

/// <summary>Smart pointer reference to a list of actor slots.</summary>
typedef OMPtr<IActorSlotList> ActorSlotListPtr;

/// <summary>Constant smart pointer reference to a list of actor slots.</summary>
typedef OMPtr<const IActorSlotList> ActorSlotListConstPtr;

/// <summary>Iterator for a list of actor slots.</summary>
typedef ListIterator<ActorSlot> ActorSlotListIterator;

/// <summary>Constant iterator for a list of actor slots.</summary>
typedef ListConstIterator<ActorSlot> ActorSlotListConstIterator;

/// <summary>Reverse iterator for a list of actor slots.</summary>
typedef std::reverse_iterator<ActorSlotListIterator>
  ActorSlotListReverseIterator;

/// <summary>Constant reverse iterator for a list of actor slots.</summary>
typedef std::reverse_iterator<ActorSlotListConstIterator>
  ActorSlotListConstReverseIterator;

/// <summary>A list of camera property values.</summary>
typedef IListExtra<CameraPropertyValue> ICameraPropertyValueList;

/// <summary>Smart pointer reference to a list of camera property values.</summary>
typedef OMPtr<ICameraPropertyValueList> CameraPropertyValueListPtr;

/// <summary>Constant smart pointer reference to a list of camera property
///   values.
/// </summary>
typedef OMPtr<const ICameraPropertyValueList> CameraPropertyValueListConstPtr;

/// <summary>Iterator for a list of camera property values.</summary>
typedef ListIterator<CameraPropertyValue> CameraPropertyValueListIterator;

/// <summary>Constant iterator for a list of camera property values.</summary>
typedef ListConstIterator<CameraPropertyValue>
  CameraPropertyValueListConstIterator;

/// <summary>Reverse iterator for a list of camera property values.</summary>
typedef std::reverse_iterator<CameraPropertyValueListIterator>
  CameraPropertyValueListConstReverseIterator;

/// <summary>A list of notifications.</summary>
typedef IListExtra<Notification> INotificationList;

/// <summary>Smart pointer reference to a list of notifications.</summary>
typedef OMPtr<INotificationList> NotificationListPtr;

/// <summary>Constant smart pointer reference to a list of
///   notifications.
/// </summary>
typedef OMPtr<const INotificationList> NotificationListConstPtr;

/// <summary>Iterator for a list of notifications.</summary>
typedef ListIterator<Notification> NotificationListIterator;

/// <summary>Constant iterator for a list of notifications.</summary>
typedef ListConstIterator<Notification> NotificationListConstIterator;

/// <summary>Reverse iterator for a list of notifications.</summary>
typedef std::reverse_iterator<NotificationListIterator>
  NotificationListReverseIterator;

/// <summary>Constant reverse iterator for a list of notifications.</summary>
typedef std::reverse_iterator<NotificationListConstIterator>
  NotificationListConstReverseIterator;

/// <summary>A list of device driver information.</summary>
typedef IListExtra<DriverInfo> IDriverInfoList;

/// <summary>Smart pointer reference to a list of device driver
///   information.
/// </summary>
typedef OMPtr<IDriverInfoList> DriverInfoListPtr;

/// <summary>Constant smart pointer reference to a list of device driver
///   information.
/// </summary>
typedef OMPtr<const IDriverInfoList> DriverInfoListConstPtr;

/// <summary>Iterator for a list of device driver information.</summary>
typedef ListIterator<DriverInfo> DriverInfoListIterator;

/// <summary>Constant iterator for a list of device driver information.</summary>
typedef ListConstIterator<DriverInfo> DriverInfoListConstIterator;

/// <summary>Reverse iterator for a list of device driver information.</summary>
typedef std::reverse_iterator<DriverInfoListIterator>
  DriverInfoListReverseIterator;

/// <summary>Constant reverse iterator for a list of device driver
///   information.
/// </summary>
typedef std::reverse_iterator<DriverInfoListConstIterator>
  DriverInfoListConstReverseIterator;

/// <summary>A list of inertial sensor data.</summary>
typedef IListExtra<InertialSensorData> IInertialSensorDataList;

/// <summary>Smart pointer reference to a list of inertial sensor data.</summary>
typedef OMPtr<IInertialSensorDataList> InertialSensorDataListPtr;

/// <summary>Constant smart pointer reference to a list of inertial sensor data.</summary>
typedef OMPtr<const IInertialSensorDataList> InertialSensorDataListConstPtr;

/// <summary>Iterator for a list of inertial sensor data.</summary>
typedef ListIterator<InertialSensorData> InertialSensorDataListIterator;

/// <summary>Constant iterator for a list of inertial sensor data.</summary>
typedef ListConstIterator<InertialSensorData> InertialSensorDataListConstIterator;

/// <summary>Reverse iterator for a list of inertial sensor data.</summary>
typedef std::reverse_iterator<InertialSensorDataListIterator>
  InertialSensorDataListReverseIterator;

/// <summary>Constant reverse iterator for a list of inertial sensor data.</summary>
typedef std::reverse_iterator<InertialSensorDataListConstIterator>
  InertialSensorDataListConstReverseIterator;

/// <summary>Create an integer list.</summary>
///
/// <returns>An empty list of integers.</summary>
IntListPtr CreateIntList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IIntList* OMCALL _CreateIntList();
#endif

/// <summary>Create an unsigned integer list.</summary>
///
/// <returns>An empty list of unsigned integers.</returns>
UIntListPtr CreateUIntList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IUIntList* OMCALL _CreateUIntList();
#endif

/// <summary>Create a Boolean list.</summary>
///
/// <returns>An empty list of Boolean values.</returns>
BoolListPtr CreateBoolList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IBoolList* OMCALL _CreateBoolList();
#endif

/// <summary>Create a string list.</summary>
///
/// <returns>An empty list of strings.</returns>
StringListPtr CreateStringList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IStringList* OMCALL _CreateStringList();
#endif

/// <summary>Create an actor data list.</summary>
///
/// <returns>An empty list of actor data.</returns>
ActorDataListPtr CreateActorDataList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorDataList* OMCALL _CreateActorDataList();
#endif

/// <summary>Create an actor slot list.</summary>
///
/// <returns>An empty list of actor slots.</summary>
ActorSlotListPtr CreateActorSlotList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IActorSlotList* OMCALL _CreateActorSlotList();
#endif

/// <summary>Create a camera property value list.</summary>
///
/// <returns>An empty list of camera property values.</returns>
CameraPropertyValueListPtr CreateCameraPropertyValueList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC ICameraPropertyValueList* OMCALL _CreateCameraPropertyValueList();
#endif

/// <summary>Create a notification list.</summary>
///
/// <returns>An empty list of notifications.</returns>
NotificationListPtr CreateNotificationList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC INotificationList* OMCALL _CreateNotificationList();
#endif

/// <summary>Create a device driver information list.</summary>
///
/// <returns>An empty list of device driver information.</returns>
DriverInfoListPtr CreateDriverInfoList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IDriverInfoList* OMCALL _CreateDriverInfoList();
#endif

/// <summary>Create an inertial sensor data list.</summary>
///
/// <returns>An empty list of inertial sensor data.</returns>
InertialSensorDataListPtr CreateInertialSensorDataList();
#ifndef DOXYGEN_INVOKED
extern "C" OMSDK2DECLSPEC IInertialSensorDataList* OMCALL _CreateInertialSensorDataList();
#endif

} // end ns sdk2
} // end ns om

#include "om/sdk2/list.inl"

#endif //OM_SDK2_LIST_H
