//--------------------------------------------------------------------------------------
//
//  File:       YPPMatchValue.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for a pattern matcher for simple values.
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

#if (! defined(YPPMATCHVALUE_H_))
# define YPPMATCHVALUE_H_ /* */

# include "YPPBaseMatcher.h"

namespace YarpPlusPlusParser
{
    /*! @brief A pattern matcher for simple values. */
    class MatchValue : public BaseMatcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchValue(void);
        
        /*! @brief Generate a proper SQL string value corresponding to this match value.
         @returns A string representing the value as a string suitable for use with SQL. */
        yarp::os::ConstString asSQLString(void)
        const;
        
        /*! @brief Return the match value as a printable string.
         @returns The matching substring as a printable string. */
        yarp::os::ConstString asString(void)
        const;

        /*! @brief Return @c true if the string has wildcard characters.
         @returns @c true if there are wildcard characters in the string and @c false otherwise. */
        bool hasWildcardCharacters(void)
        const
        {
            return _hasWildcards;
        } // hasWildcardCharacters
        
        /*! @brief Return @c true if the string will be escaped during conversion to SQL.
         @returns @c true if there are characters in the string that will be escaped during conversin and @c false
         otherwise. */
        bool willBeEscapedDuringConversion(void)
        const
        {
            return _needsEscaping;
        } // willBeEscapedDuringConversion
        
        /*! @brief Create a pattern matcher if the next substring would be a valid value.
         @param inString The string being scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchValue * CreateMatcher(const yarp::os::ConstString & inString,
                                          const int                     inLength,
                                          const int                     startPos,
                                          int &                         endPos);
                
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseMatcher inherited;
        
        /*! @brief The constructor.
         @param inString The matching segment of the original string. */
        MatchValue(const yarp::os::ConstString & inString);
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchValue(const MatchValue & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchValue & operator=(const MatchValue & other);
        
        /*! @brief The subtring that (maximally) matched as a value. */
        yarp::os::ConstString _matchingString;
        /*! @brief @c true if the string has single quote characters and @c false otherwise. */
        bool                  _hasSingleQuotes;
        /*! @brief @c true if the string has wild-card characters and @c false otherwise. */
        bool                  _hasWildcards;
        /*! @brief @c true if the string will need to be escaped and @c false otherwise. */
        bool                  _needsEscaping;
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char                  _filler[5];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
        
    }; // MatchValue
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHVALUE_H_)