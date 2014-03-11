//
//  YPPMatchExpression.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-10.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHEXPRESSION_H_))
# define YPPMATCHEXPRESSION_H_ /* */

# include "YPPBaseMatcher.h"
# include <vector>

namespace YarpPlusPlusParser
{
    class MatchConstraint;
    
    /*! @brief A pattern matcher for field/values pairs. */
    class MatchExpression : public BaseMatcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchExpression(void);
        
        /*! @brief Generate a proper SQL string value corresponding to this match value.
         @param prefixString The SELECT prefix to be applied before each expression.
         @returns A string representing the value as a string suitable for use with SQL. */
        yarp::os::ConstString asSQLString(const char * prefixString)
        const;
        
        /*! @brief Return the match value as a printable string.
         @returns The matching substring as a printable string. */
        yarp::os::ConstString asString(void)
        const;

        /*! @brief Return the number of elements in the expression list.
         @returns The number of elements in the expression list. */
        int count(void)
        const;
        
        /*! @brief Return an element from the expression list.
         @param index The zero-origin index of the element.
         @returns An element of the expression list or @c NULL if the index is outside the range of the expression
         list. */
        const MatchConstraint * element(const int index)
        const;
        
        /*! @brief Create a pattern matcher if the next substring would be a valid expression.
         @param inString The string being scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @param validator A function that returns @c true if the field name is valid and @c false otherwise.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchExpression * createMatcher(const yarp::os::ConstString & inString,
                                               const int                     inLength,
                                               const int                     startPos,
                                               int &                         endPos,
                                               FieldNameValidator            validator = NULL);
        
        /*! @brief The character used between expression list elements.
         @returns The character that separates expression list elements. */
        static char expressionSeparatorCharacter(void);
        
    protected:
        
    private:
        
        typedef BaseMatcher inherited;
        
        typedef std::vector<MatchConstraint *> MatchExpressionListData;
        
        typedef MatchExpressionListData::size_type MatchExpressionListSize;
        
        /*! @brief The constructor. */
        MatchExpression(void);
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchExpression(const MatchExpression & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchExpression & operator=(const MatchExpression & other);
        
        /*! @brief Remove all the list elements. */
        void empty(void);
        
        /*! @brief The elements of the list. */
        MatchExpressionListData _constraints;
        
    }; // MatchExpression
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHEXPRESSION_H_)
