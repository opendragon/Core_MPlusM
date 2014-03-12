//
//  YPPBaseMatcher.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

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
         const char * * prefixString,
         const char * * suffixString);
    
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
