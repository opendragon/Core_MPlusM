//
//  YPPMatchFieldName.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHFIELDNAME_H_))
# define YPPMATCHFIELDNAME_H_ /* */

# include "YPPMatcher.h"
# include <yarp/os/ConstString.h>

namespace YarpPlusPlusParser
{
    /*! @brief A pattern matcher for simple values. */
    class MatchFieldName : public Matcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~MatchFieldName(void);
        
        /*! @brief Return the match value.
         @returns The matching substring. */
        yarp::os::ConstString asString(void)
        const
        {
            return _matchingString;
        } // asString

        /*! @brief Create a pattern matcher if the next substring would be a valid value.
         @param inString The string being scanned.
         @param startPos Where in the string to start scanning.
         @param endPos Where the scan terminated, if successful.
         @returns A non-null matcher if the string would be a valid value and @c NULL otherwise. */
        static MatchFieldName * createMatcher(const yarp::os::ConstString & inString,
                                              const int                     startPos,
                                              int &                         endPos);
        
    protected:
        
    private:
        
        typedef Matcher inherited;
        
        /*! @brief The constructor.
         @param inString The matching segment of the original string. */
        MatchFieldName(const yarp::os::ConstString & inString);
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldName(const MatchFieldName & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        MatchFieldName & operator=(const MatchFieldName & other);
        
        /*! @brief The subtring that (maximally) matched as a value. */
        yarp::os::ConstString _matchingString;
        
    }; // MatchFieldName
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHFIELDNAME_H_)
