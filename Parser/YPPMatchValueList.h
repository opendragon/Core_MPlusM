//--------------------------------------------------------------------------------------
//
//  File:       YPPMatchValueList.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for a pattern matcher for lists of simple values.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by OpenDragon.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-03-07
//
//--------------------------------------------------------------------------------------

#if (! defined(YPPMATCHVALUELIST_H_))
# define YPPMATCHVALUELIST_H_ /* */

# include "YPPBaseMatcher.h"
# include <vector>

namespace YarpPlusPlusParser
{
    class MatchValue;
    
    /*! @brief A pattern matcher for lists of simple values. */
    class MatchValueList : public BaseMatcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchValueList(void);
        
        /*! @brief Generate a proper SQL string value corresponding to this match value.
         @param fieldName The name to be used in the SQL matching expression.
         @returns A string representing the value as a string suitable for use with SQL. */
        yarp::os::ConstString asSQLString(const char * fieldName)
        const;

        /*! @brief Create a printable representation of the value list.
         @returns A printable representation of the value list. */
        const yarp::os::ConstString asString(void)
        const;
        
        /*! @brief Return the number of elements in the value list.
         @returns The number of elements in the value list. */
        int count(void)
        const;
        
        /*! @brief Return an element from the value list.
         @param index The zero-origin index of the element.
         @returns An element of the value list or @c NULL if the index is outside the range of the value list. */
        const MatchValue * element(const int index)
        const;

        /*! @brief Create a pattern matcher if the next substring would be a valid value list.
         @param inString The string being scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchValueList * CreateMatcher(const yarp::os::ConstString & inString,
                                              const int                     inLength,
                                              const int                     startPos,
                                              int &                         endPos);
        
        /*! @brief The character used to signal the beginning of a value list.
         @returns The character that ends a value list. */
        static char ListInitiatorCharacter(void);

        /*! @brief The character used between value list elements.
         @returns The character that separates value list elements. */
        static char ListSeparatorCharacter(void);
        
        /*! @brief The character used to signal the end of a value list.
         @returns The character that ends a value list. */
        static char ListTerminatorCharacter(void);

    protected:
        
        /*! @brief The constructor. */
        MatchValueList(void);
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseMatcher inherited;
        
        /*! @brief A sequence of values. */
        typedef std::vector<MatchValue *>     MatchValueListData;
        
        /*! @brief The size-type for sequence data. */
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
