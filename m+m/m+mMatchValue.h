//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mMatchValue.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a pattern matcher for simple values.
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

#if (! defined(MpMMatchValue_H_))
# define MpMMatchValue_H_ /* Header guard */

# include <m+m/m+mBaseMatcher.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a pattern matcher for simple values. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Parser
    {
        /*! @brief A pattern matcher for simple values. */
        class MatchValue : public BaseMatcher
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseMatcher inherited;

        public :

            /*! @brief The destructor. */
            virtual
            ~MatchValue(void);

            /*! @brief Generate a proper SQL string value corresponding to this match value.
             @returns A string representing the value as a string suitable for use with SQL. */
            YarpString
            asSQLString(void)
            const;

            /*! @brief Return the match value as a printable string.
             @returns The matching substring as a printable string. */
            YarpString
            asString(void)
            const;

            /*! @brief Create a pattern matcher if the next substring would be a valid value.
             @param inString The string being scanned.
             @param inLength The length of the string being scanned.
             @param startPos Where in the string to start scanning.
             @param endPos Where the scan terminated, if successful.
             @returns A non-null matcher if the string would be a valid value and @c NULL
             otherwise. */
            static MatchValue *
            CreateMatcher(const YarpString & inString,
                          const size_t       inLength,
                          const size_t       startPos,
                          size_t &           endPos);

            /*! @brief Return @c true if the string has wildcard characters.
             @returns @c true if there are wildcard characters in the string and @c false
             otherwise. */
            inline bool
            hasWildcardCharacters(void)
            const
            {
                return _hasWildcards;
            } // hasWildcardCharacters

        protected :

        private :

            /*! @brief The constructor.
             @param inString The matching segment of the original string. */
            explicit
            MatchValue(const YarpString & inString);

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            MatchValue(const MatchValue & other);
            
            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            MatchValue &
            operator =(const MatchValue & other);

        public :

        protected :

        private :

            /*! @brief The subtring that (maximally) matched as a value. */
            YarpString _matchingString;

            /*! @brief @c true if the string has single quote characters and @c false otherwise. */
            bool _hasSingleQuotes;

            /*! @brief @c true if the string has wild-card characters and @c false otherwise. */
            bool _hasWildcards;

            /*! @brief @c true if the string will need to be escaped and @c false otherwise. */
            bool _needsEscaping;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[5];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // MatchValue

    } // Parser

} // MplusM

#endif // ! defined(MpMMatchValue_H_)
