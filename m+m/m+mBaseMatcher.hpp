//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseMatcher.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required for an m+m pattern
//              matcher.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2014-03-07
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseMatcher_HPP_))
# define MpMBaseMatcher_HPP_ /* Header guard */

# include <m+m/m+mCommon.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an m+m pattern matcher. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Parser
    {
        /*! @brief The common functionality for pattern matchers. */
        class BaseMatcher
        {
        public :

        protected :

        private :

        public :

            /*! @brief The destructor. */
            virtual
            ~BaseMatcher(void);

        protected :

            /*! @brief The constructor. */
            BaseMatcher(void);

            /*! @brief Scan a string for the next non-whitespace character.
             @param[in] inString The string to be scanned.
             @param[in] inLength The length of the string being scanned.
             @param[in] startPos Where in the string to begin scanning.
             @return The position in the string where the next non-whitespace character appears, or
             the length of the string - which is past the end of the string - if the remainder of
             the string is whitespace. */
            static size_t
            SkipWhitespace(const YarpString & inString,
                           const size_t       inLength,
                           const size_t       startPos);

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            BaseMatcher(const BaseMatcher & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            BaseMatcher &
            operator =(const BaseMatcher & other);

        public :

        protected :

        private :

        }; // BaseMatcher

    } // Parser

} // MplusM

#endif // ! defined(MpMBaseMatcher_HPP_)
