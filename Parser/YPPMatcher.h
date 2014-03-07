//
//  YPPMatcher.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-03-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPMATCHER_H_))
# define YPPMATCHER_H_ /* */

# include "YPPConfig.h"
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
