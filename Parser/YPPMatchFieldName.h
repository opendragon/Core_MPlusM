//--------------------------------------------------------------------------------------
//
//  File:       YPPMatchFieldName.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for a pattern matcher for field names.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
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

#if (! defined(YPPMATCHFIELDNAME_H_))
# define YPPMATCHFIELDNAME_H_ /* */

# include "YPPBaseMatcher.h"

namespace YarpPlusPlusParser
{
    class BaseNameValidator;
    
    /*! @brief A pattern matcher for field names. */
    class MatchFieldName : public BaseMatcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchFieldName(void);
        
        /*! @brief Generate a proper SQL string value corresponding to this match value.
         @returns A string representing the value as a string suitable for use with SQL. */
        const yarp::os::ConstString & asSQLString(void)
        const
        {
            return _matchingString;
        } // asSQLString
        
        /*! @brief Return the match value.
         @returns The matching substring. */
        const yarp::os::ConstString & asString(void)
        const
        {
            return _matchingString;
        } // asString

        /*! @brief Create a pattern matcher if the next substring would be a valid value.
         @param inString The string being scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @param validator A function that returns @c true if the field name is valid and @c false otherwise.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchFieldName * CreateMatcher(const yarp::os::ConstString & inString,
                                              const int                     inLength,
                                              const int                     startPos,
                                              int &                         endPos,
                                              BaseNameValidator *           validator = NULL);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseMatcher inherited;
        
        /*! @brief The constructor.
         @param inString The matching segment of the original string. */
        MatchFieldName(const yarp::os::ConstString & inString);
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldName(const MatchFieldName & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldName & operator=(const MatchFieldName & other);
        
        /*! @brief The subtring that (maximally) matched as a value. */
        yarp::os::ConstString _matchingString;
        
    }; // MatchFieldName
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHFIELDNAME_H_)
