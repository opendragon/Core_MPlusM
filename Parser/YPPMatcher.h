//
//  YPPMatcher.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHER_H_))
# define YPPMATCHER_H_ /* */

# include "YPPCommon.h"
# include <yarp/os/ConstString.h>

namespace YarpPlusPlusParser
{
    /*! @brief A pattern matcher for simple values. */
    class Matcher
    {
    public:
        
        /*! @brief The destructor. */
        virtual ~Matcher(void);
        
    protected:
        
        /*! @brief The constructor. */
        Matcher(void);
        
        /*! @brief Scan a string for the next non-whitespace character.
         @param inString The string to be scanned.
         @param inLength The length of the string being scanned.
         @param startPos Where in the string to begin scanning.
         @returns The position in the string where the next non-whitespace character appears, or the length of the
         string - which is past the end of the string - if the remainder of the string is whitespace. */
        static int skipWhitespace(const yarp::os::ConstString & inString,
                                  const int                     inLength,
                                  const int                     startPos);
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        Matcher(const Matcher & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        Matcher & operator=(const Matcher & other);
        
    }; // Matcher
    
} // YarpPlusPlusParser

#endif // ! defined(YPPMATCHER_H_)
