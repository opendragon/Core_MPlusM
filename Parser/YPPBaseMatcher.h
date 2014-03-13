//--------------------------------------------------------------------------------------
//
//  File:       YPPBaseMatcher.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for the minimal functionality required for a Yarp++
//              pattern matcher.
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

#if (! defined(YPPBASEMATCHER_H_))
# define YPPBASEMATCHER_H_ /* */

# include "YPPCommon.h"

namespace YarpPlusPlusParser
{
    /*! @brief A function that checks field names for validity.
     @param aString The string to be checked.
     @param prefixString If non-@c NULL, a pointer to the string to be used in the SQL prefix for this field.
     @param suffixString If non-@c NULL, a pointer to the string to be used in the SQL suffix for this field.
     @returns The actual field name to be used or @c NULL if the field name was unmatched. */
    typedef const char * (*FieldNameValidator)
        (const char *   aString,
         const char **  prefixString,
         const char **  suffixString);
    
    /*! @brief The common functionality for pattern matchers. */
    class BaseMatcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~BaseMatcher(void);
        
    protected:
        
        /*! @brief The constructor. */
        BaseMatcher(void);
        
        /*! @brief Scan a string for the next non-whitespace character.
         @param inString The string to be scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to begin scanning.
         @returns The position in the string where the next non-whitespace character appears, or the length of the
         string - which is past the end of the string - if the remainder of the string is whitespace. */
        static int SkipWhitespace(const yarp::os::ConstString & inString,
                                  const int                     inLength,
                                  const int                     startPos);
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseMatcher(const BaseMatcher & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseMatcher & operator=(const BaseMatcher & other);
        
    }; // BaseMatcher
    
} // YarpPlusPlusParser

#endif // ! defined(YPPBASEMATCHER_H_)