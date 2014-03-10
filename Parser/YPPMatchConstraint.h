//
//  YPPMatchConstraint.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-10.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHCONSTRAINT_H_))
# define YPPMATCHCONSTRAINT_H_ /* */

# include "YPPMatcher.h"
# include <vector>

namespace YarpPlusPlusParser
{
    class MatchFieldWithValues;
    
    /*! @brief A pattern matcher for field/values pairs. */
    class MatchConstraint : public Matcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchConstraint(void);
        
        /*! @brief Return the match value as a printable string.
         @returns The matching substring as a printable string. */
        yarp::os::ConstString asString(void)
        const;

        /*! @brief The character used between constraint list elements.
         @returns The character that separates constraint list elements. */
        static char constraintSeparatorCharacter(void);
        
        /*! @brief Return the number of elements in the list.
         @returns The number of elements in the list. */
        int count(void)
        const;
        
        /*! @brief Return an element from the list.
         @param index The zero-origin index of the element.
         @returns An element of the list or @c NULL if the index is outside the range of the list. */
        const MatchFieldWithValues * element(const int index)
        const;
        
        /*! @brief Create a pattern matcher if the next substring would be a valid value.
         @param inString The string being scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @param validator A function that returns @c true if the field name is valid and @c false otherwise.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchConstraint * createMatcher(const yarp::os::ConstString & inString,
                                               const int                     inLength,
                                               const int                     startPos,
                                               int &                         endPos,
                                               FieldNameValidator            validator = NULL);
        
    protected:
        
    private:
        
        typedef Matcher inherited;
        
        typedef std::vector<MatchFieldWithValues *> MatchConstraintListData;
        
        typedef MatchConstraintListData::size_type MatchConstraintListSize;
        
        /*! @brief The constructor. */
        MatchConstraint(void);
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchConstraint(const MatchConstraint & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchConstraint & operator=(const MatchConstraint & other);
        
        /*! @brief Remove all the list elements. */
        void empty(void);
        
        /*! @brief The elements of the list. */
        MatchConstraintListData _constraints;
        
    }; // MatchConstraint
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHCONSTRAINT_H_)
