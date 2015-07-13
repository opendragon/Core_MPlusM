//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mMatchExpression.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a pattern matcher for expressions.
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
//  Created:    2014-03-10
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMMatchExpression_H_))
# define MpMMatchExpression_H_ /* Header guard */

# include <m+m/m+mBaseMatcher.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a pattern matcher for expressions. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Parser
    {
        class BaseNameValidator;
        class MatchConstraint;
        
        /*! @brief A pattern matcher for expressions. */
        class MatchExpression : public BaseMatcher
        {
        public :
            
            /*! @brief The destructor. */
            virtual ~MatchExpression(void);
            
            /*! @brief Generate a proper SQL string value corresponding to this match value.
             @param prefixString The SELECT prefix to be applied before each expression.
             @param suffixString The SELECT suffix to be applied after each expression.
             @returns A string representing the value as a string suitable for use with SQL. */
            YarpString asSQLString(const char * prefixString,
                                   const char * suffixString = NULL)
            const;
            
            /*! @brief Return the match value as a printable string.
             @returns The matching substring as a printable string. */
            YarpString asString(void)
            const;
            
            /*! @brief Return the number of elements in the expression list.
             @returns The number of elements in the expression list. */
            int count(void)
            const;
            
            /*! @brief Create a pattern matcher if the next substring would be a valid expression.
             @param inString The string being scanned.
             @param inLength The length of the string being scanned.
             @param startPos Where in the string to start scanning.
             @param endPos Where the scan terminated, if successful.
             @param validator A function that returns @c true if the field name is valid and @c
             false otherwise.
             @returns A non-null matcher if the string would be a valid value and @c NULL
             otherwise. */
            static MatchExpression * CreateMatcher(const YarpString &  inString,
                                                   const size_t        inLength,
                                                   const size_t        startPos,
                                                   size_t &            endPos,
                                                   BaseNameValidator * validator = NULL);
            
            /*! @brief Return an element from the expression list.
             @param index The zero-origin index of the element.
             @returns An element of the expression list or @c NULL if the index is outside the range
             of the expression list. */
            const MatchConstraint * element(const int index)
            const;
            
            /*! @brief The character used between expression list elements.
             @returns The character that separates expression list elements. */
            static char ExpressionSeparatorCharacter(void);
            
        protected :
            
        private :
            
            /*! @brief The constructor. */
            MatchExpression(void);
            
            COPY_AND_ASSIGNMENT_(MatchExpression);
            
            /*! @brief Remove all the list elements. */
            void empty(void);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseMatcher inherited;
            
            /*! @brief A sequence of AND clauses. */
            typedef std::vector<MatchConstraint *> MatchExpressionListData;
            
            /*! @brief The size-type for sequence data. */
            typedef MatchExpressionListData::size_type MatchExpressionListSize;
            
            /*! @brief The elements of the list. */
            MatchExpressionListData _constraints;
            
        }; // MatchExpression
        
    } // Parser
    
} // MplusM

#endif // ! defined(MpMMatchExpression_H_)
