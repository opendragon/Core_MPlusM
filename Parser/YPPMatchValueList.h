//
//  YPPMatchValueList.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHVALUELIST_H_))
# define YPPMATCHVALUELIST_H_ /* */

# include "YPPMatcher.h"
# include <vector>

namespace YarpPlusPlusParser
{
    class MatchValue;
    
    /*! @brief A pattern matcher for simple values. */
    class MatchValueList : public Matcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchValueList(void);
        
        /*! @brief Create a printable representation of the value list.
         @returns A printable representation of the list. */
        const yarp::os::ConstString asString(void)
        const;
        
        /*! @brief Return the number of elements in the list.
         @returns The number of elements in the list. */
        int count(void)
        const;
        
        /*! @brief Return an element from the list.
         @param index The zero-origin index of the element.
         @returns An element of the list or @c NULL if the index is outside the range of the list. */
        const MatchValue * element(const int index)
        const;

        /*! @brief Create a pattern matcher if the next substring would be a valid value list.
         @param inString The string being scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchValueList * createMatcher(const yarp::os::ConstString & inString,
                                              const int                     inLength,
                                              const int                     startPos,
                                              int &                         endPos);
        
        /*! @brief The character used to signal the beginning of a value list.
         @returns The character that ends a value list. */
        static char listInitiatorCharacter(void);

        /*! @brief The character used between value list elements.
         @returns The character that separates value list elements. */
        static char listSeparatorCharacter(void);
        
        /*! @brief The character used to signal the end of a value list.
         @returns The character that ends a value list. */
        static char listTerminatorCharacter(void);

    protected:
        
        /*! @brief The constructor. */
        MatchValueList(void);
        
    private:
        
        typedef Matcher inherited;
        
        typedef std::vector<MatchValue *> MatchValueListData;
        
        typedef MatchValueListData::size_type MatchValueListSize;
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchValueList(const MatchValueList & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchValueList & operator=(const MatchValueList & other);

        /*! @brief Remove all the list elements. */
        void empty(void);
        
        /*! @brief The elements of the list. */
        MatchValueListData _values;
        
    }; // MatchValueList
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHVALUELIST_H_)
