//--------------------------------------------------------------------------------------
//
//  File:       YPPExampleRunningSumClient.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for the client of a simple Yarp++ service with context.
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

#if (! defined(YPPEXAMPLERUNNINGSUMCLIENT_H_))
# define YPPEXAMPLERUNNINGSUMCLIENT_H_ /* */

# include "YPPBaseClient.h"
# include <yarp/os/Port.h>

namespace YarpPlusPlusExample
{
    /*! @brief An example Yarp++ client, for the 'random' service. */
    class ExampleRunningSumClient : public YarpPlusPlus::BaseClient
    {
    public:
        
        /*! @brief The constructor. */
        ExampleRunningSumClient(void);
        
        /*! @brief The destructor. */
        virtual ~ExampleRunningSumClient(void);
        
        /*! @brief Reset the running sum for this client.
         @param value The value to add to the running sum.
         @param newSum The new running sum.
         @returns @c true if the service handled the request and @c false otherwise. */
        bool addToSum(const double value,
                      double &     newSum);
        
        /*! @brief Reset the running sum for this client.
         @returns @c true if the service handled the request and @c false otherwise. */
        bool resetSum(void);
        
        /*! @brief Start the running sum for this client.
         @returns @c true if the service handled the request and @c false otherwise. */
        bool startSum(void);
        
        /*! @brief Stop the running sum for this client.
         @returns @c true if the service handled the request and @c false otherwise. */
        bool stopSum(void);

    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseClient inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleRunningSumClient(const ExampleRunningSumClient & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleRunningSumClient & operator=(const ExampleRunningSumClient & other);
        
        /*! @brief The YARP port to be used by the client. */
        yarp::os::Port * _port;

    }; // ExampleRunningSumClient
    
} // YarpPlusPlusExample

#endif // ! defined(YPPEXAMPLERUNNINGSUMCLIENT_H_)
