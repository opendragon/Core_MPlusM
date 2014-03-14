//--------------------------------------------------------------------------------------
//                                                                                      
//  File:       YPPBaseClient.h
//
//  Project:    YarpPlusPlus
//                                                                                      
//  Contains:   The class declaration for the minimal functionality required for a Yarp++
//              client.
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
//  Created:    2014-02-06
//                                                                                      
//--------------------------------------------------------------------------------------

#if (! defined(YPPBASECLIENT_H_))
# define YPPBASECLIENT_H_ /* */

# include "YPPConfig.h"
# include <yarp/os/Bottle.h>
# include <yarp/os/Port.h>

namespace YarpPlusPlus
{
    class ServiceResponse;
    
    /*! @brief The minimal functionality required for a Yarp++ client. */
    class BaseClient
    {
    public:
        
        /*! @brief The number of elements expected in the output of FindMatchingServices. */
        static const int kExpectedResponseSize;
        
        /*! @brief The constructor. */
        BaseClient(void);
        
        /*! @brief The destructor. */
        virtual ~BaseClient(void);
        
        /*! @brief Find a matching service and prepare to send requests to it.
         @param criteria The criteria to use to locate the service.
         @param allowOnlyOneMatch @c true if only one match is allowed and @c false if the first match will be used.
         @returns @c true if a matching service was found and @c false if no matching service or too many services were
         found. */
        bool connect(const char * criteria,
                     const bool   allowOnlyOneMatch = false);
        
    protected:
        
        /*! @brief Send a request to the service associated with the client.
         @param request The name of the request.
         @param parameters The required parameters for the request.
         @param usingPort The port that is to send the request, or @c NULL if an arbitrary port is to be used.
         @param response If non-@c NULL, where to store any response received.
         @returns @c true on a successful communication with the service and @c false otherwise. */
        bool send(const char *             request,
                  const yarp::os::Bottle & parameters,
                  yarp::os::Port *         usingPort = NULL,
                  ServiceResponse *        response = NULL);
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseClient(const BaseClient & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseClient & operator=(const BaseClient & other);
        
        /*! @brief The name of the service port being used. */
        yarp::os::ConstString _servicePort;
        
    }; // BaseClient
    
    /*! @brief Find one or more matching services that are registered with a running Service Registry service.
     @param criteria The matching conditions.
     @returns A (possibly empty) list of matching services. */
    yarp::os::Bottle FindMatchingServices(const char * criteria);
    
} // YarpPlusPlus

#endif // ! defined(YPPBASECLIENT_H_)
