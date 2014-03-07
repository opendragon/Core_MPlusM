//
//  YPPMatchFieldWithValues.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHFIELDWITHVALUES_H_))
# define YPPMATCHFIELDWITHVALUES_H_ /* */

# include "YPPMatcher.h"
# include <yarp/os/ConstString.h>

namespace YarpPlusPlusParser
{
    class MatchFieldName;
    class MatchValue;
    class MatchValueList;
    
    /*! @brief A pattern matcher for field/values pairs. */
    class MatchFieldWithValues : public Matcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchFieldWithValues(void);
        
        /*! @brief Return the match value as a printable string.
         @returns The matching substring as a printable string. */
        yarp::os::ConstString asString(void)
        const;

        /*! @brief Create a pattern matcher if the next substring would be a valid value.
         @param inString The string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchFieldWithValues * createMatcher(const yarp::os::ConstString & inString,
                                                    const int                     startPos,
                                                    int &                         endPos);
        
    protected:
        
    private:
        
        typedef Matcher inherited;
        
        /*! @brief The constructor. */
        MatchFieldWithValues(MatchFieldName * fieldName,
                             MatchValue *     asSingle,
                             MatchValueList * asList);
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldWithValues(const MatchFieldWithValues & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldWithValues & operator=(const MatchFieldWithValues & other);
        
        /*! @brief The field name. */
        MatchFieldName * _fieldName;
        /*! @brief The value, if a single value is present. */
        MatchValue *     _singleValue;
        /*! @brief The list of values, if more than one value is present. */
        MatchValueList * _values;
        
    }; // MatchFieldWithValues
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHFIELDWITHVALUES_H_)
