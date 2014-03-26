//--------------------------------------------------------------------------------------
//
//  File:       YPPRunningSumContext.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for a context used with a simple Yarp++ service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-03-18
//
//--------------------------------------------------------------------------------------

#if (! defined(YPPRUNNINGSUMCONTEXT_H_))
/*! @brief Header guard. */
# define YPPRUNNINGSUMCONTEXT_H_ /* */

# include "YPPBaseContext.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a context used with a simple Yarp++ service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace YarpPlusPlusExample
{
    /*! @brief A convenience class to provide context objects for the running sum service. */
    class RunningSumContext : public YarpPlusPlus::BaseContext
    {
    public:
        
        /*! @brief The constructor. */
        RunningSumContext(void);
        
        /*! @brief The destructor. */
        virtual ~RunningSumContext(void);
        
        /*! @brief An accessor for the running sum. */
        inline double & sum(void)
        {
            return _sum;
        } // sum
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseContext inherited;
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RunningSumContext(const RunningSumContext & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RunningSumContext & operator=(const RunningSumContext & other);
        
        /*! @brief The running sum. */
        double _sum;
        
    }; // RunningSumContext
    
} // YarpPlusPlusExample

#endif // ! defined(YPPRUNNINGSUMCONTEXT_H_)
