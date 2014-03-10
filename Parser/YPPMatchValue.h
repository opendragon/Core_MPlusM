//
//  YPPMatchValue.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHVALUE_H_))
# define YPPMATCHVALUE_H_ /* */

# include "YPPBaseMatcher.h"

namespace YarpPlusPlusParser
{
    /*! @brief A pattern matcher for simple values. */
    class MatchValue : public BaseMatcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchValue(void);
        
        /*! @brief Generate a proper SQL string value corresponding to this match value.
         @returns A string representing the value as a string suitable for use with SQL. */
        yarp::os::ConstString asSQLString(void)
        const;
        
        /*! @brief Return the match value as a printable string.
         @returns The matching substring as a printable string. */
        yarp::os::ConstString asString(void)
        const;

        /*! @brief Return @c true if the string has wildcard characters.
         @returns @c true if there are wildcard characters in the string and @c false otherwise. */
        bool hasWildcardCharacters(void)
        const
        {
            return _hasWildcards;
        } // hasWildcardCharacters
        
        /*! @brief Return @c true if the string will be escaped during conversion to SQL.
         @returns @c true if there are characters in the string that will be escaped during conversin and @c false
         otherwise. */
        bool willBeEscapedDuringConversion(void)
        const
        {
            return _needsEscaping;
        } // willBeEscapedDuringConversion
        
        /*! @brief Create a pattern matcher if the next substring would be a valid value.
         @param inString The string being scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchValue * createMatcher(const yarp::os::ConstString & inString,
                                          const int                     inLength,
                                          const int                     startPos,
                                          int &                         endPos);
                
    protected:
        
    private:
        
        typedef BaseMatcher inherited;
        
        /*! @brief The constructor.
         @param inString The matching segment of the original string. */
        MatchValue(const yarp::os::ConstString & inString);
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchValue(const MatchValue & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchValue & operator=(const MatchValue & other);
        
        /*! @brief The subtring that (maximally) matched as a value. */
        yarp::os::ConstString _matchingString;
        /*! @brief @c true if the string has single quote characters and @c false otherwise. */
        bool                  _hasSingleQuotes;
        /*! @brief @c true if the string has wild-card characters and @c false otherwise. */
        bool                  _hasWildcards;
        /*! @brief @c true if the string will need to be escaped and @c false otherwise. */
        bool                  _needsEscaping;
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char                  _filler[5];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
        
    }; // MatchValue
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHVALUE_H_)
