//
//  YPPMatchFieldWithValues.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHFIELDWITHVALUES_H_))
# define YPPMATCHFIELDWITHVALUES_H_ /* */

# include "YPPBaseMatcher.h"

namespace YarpPlusPlusParser
{
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
                                                    const int                     inLength,
                                                    const int                     startPos,
                                                    int &                         endPos,
                                                    FieldNameValidator            validator = NULL);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseMatcher inherited;
        
        /*! @brief The constructor.
         @param validator A function that returns @c true if the field name is valid and @c false otherwise.
         @param fieldName The name of the field.
         @param asSingle The value for the field. */
        MatchFieldWithValues(FieldNameValidator validator,
                             MatchFieldName *   fieldName,
                             MatchValue *       asSingle);
        
        /*! @brief The constructor.
         @param validator A function that returns @c true if the field name is valid and @c false otherwise.
         @param fieldName The name of the field.
         @param asList The list of values for the field. */
        MatchFieldWithValues(FieldNameValidator validator,
                             MatchFieldName *   fieldName,
                             MatchValueList *   asList);

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldWithValues(const MatchFieldWithValues & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldWithValues & operator=(const MatchFieldWithValues & other);
        
        /*! @brief The validator function that was used for this field. */
        FieldNameValidator _validator;
        /*! @brief The field name. */
        MatchFieldName *   _fieldName;
        /*! @brief The value, if a single value is present. */
        MatchValue *       _singleValue;
        /*! @brief The list of values, if more than one value is present. */
        MatchValueList *   _values;
        
    }; // MatchFieldWithValues
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHFIELDWITHVALUES_H_)
