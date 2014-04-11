//--------------------------------------------------------------------------------------
//
//  File:       MoMeMatchFieldWithValues.h
//
//  Project:    MoAndMe
//
//  Contains:   The class declaration for a pattern matcher for field/value pairs.
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

#if (! defined(MOMEMATCHFIELDWITHVALUES_H_))
/*! @brief Header guard. */
# define MOMEMATCHFIELDWITHVALUES_H_ /* */

# include "MoMeBaseMatcher.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a pattern matcher for field/value pairs. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MoAndMe
{
    namespace Parser
    {
        class BaseNameValidator;
        class MatchFieldName;
        class MatchValue;
        class MatchValueList;
        
        /*! @brief A pattern matcher for field/values pairs. */
        class MatchFieldWithValues : public BaseMatcher
        {
        public:
            
            /*! @brief The destructor. */
            virtual ~MatchFieldWithValues(void);
            
            /*! @brief Generate a proper SQL string value corresponding to this match value.
             @returns A string representing the value as a string suitable for use with SQL. */
            yarp::os::ConstString asSQLString(void)
            const;
            
            /*! @brief Return the match value as a printable string.
             @returns The matching substring as a printable string. */
            yarp::os::ConstString asString(void)
            const;
            
            /*! @brief Create a pattern matcher if the next substring would be a valid field with value(s).
             @param inString The string being scanned.
             @param inLength The length of the string being scanned.
             @param startPos Where in the string to start scanning.
             @param endPos Where the scan terminated, if successful.
             @param validator A function that returns @c true if the field name is valid and @c false otherwise.
             @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
            static MatchFieldWithValues * CreateMatcher(const yarp::os::ConstString & inString,
                                                        const size_t                  inLength,
                                                        const size_t                  startPos,
                                                        size_t &                      endPos,
                                                        BaseNameValidator *           validator = NULL);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseMatcher inherited;
            
            /*! @brief The constructor.
             @param validator A function that returns @c true if the field name is valid and @c false otherwise.
             @param fieldName The name of the field.
             @param asSingle The value for the field. */
            MatchFieldWithValues(BaseNameValidator * validator,
                                 MatchFieldName *    fieldName,
                                 MatchValue *        asSingle);
            
            /*! @brief The constructor.
             @param validator A function that returns @c true if the field name is valid and @c false otherwise.
             @param fieldName The name of the field.
             @param asList The list of values for the field. */
            MatchFieldWithValues(BaseNameValidator * validator,
                                 MatchFieldName *    fieldName,
                                 MatchValueList *    asList);
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            MatchFieldWithValues(const MatchFieldWithValues & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            MatchFieldWithValues & operator=(const MatchFieldWithValues & other);
            
            /*! @brief The validator function object that was used for this field. */
            BaseNameValidator * _validator;
            
            /*! @brief The field name. */
            MatchFieldName *    _fieldName;
            
            /*! @brief The value, if a single value is present. */
            MatchValue *        _singleValue;
            
            /*! @brief The list of values, if more than one value is present. */
            MatchValueList *    _values;
            
        }; // MatchFieldWithValues
        
    } // Parser
    
} // MoAndMe

#endif // ! defined(MOMEMATCHFIELDWITHVALUES_H_)
