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
/*! @brief Header guard. */
# define YPPBASECLIENT_H_ /* */

# include "YPPCommon.h"
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wc++11-extensions"
#  pragma clang diagnostic ignored "-Wdocumentation"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#  pragma clang diagnostic ignored "-Wpadded"
#  pragma clang diagnostic ignored "-Wshadow"
#  pragma clang diagnostic ignored "-Wunused-parameter"
#  pragma clang diagnostic ignored "-Wweak-vtables"
# endif // defined(__APPLE__)
# include <yarp/os/Bottle.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for a Yarp++ client. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace YarpPlusPlus
{
    class ServiceResponse;
    
    /*! @brief The minimal functionality required for a Yarp++ client. */
    class BaseClient
    {
    public:
        
        /*! @brief The constructor.
         @param basePortName The name to be used as the root for the client port. */
        BaseClient(const char * basePortName = DEFAULT_PORT_ROOT);
        
        /*! @brief The destructor. */
        virtual ~BaseClient(void);
        
        /*! @brief Create a connection with the service.
         @returns @c true if the client is connected to the service and @c false otherwise. */
        bool connectToService(void);
        
        /*! @brief Disconnect from the service.
         @returns @c true if the client is no longer connected to the service and @ false otherwise. */
        bool disconnectFromService(void);
        
        /*! @brief Find a matching service and prepare to send requests to it.
         @param criteria The criteria to use to locate the service.
         @param allowOnlyOneMatch @c true if only one match is allowed and @c false if the first match will be used.
         @returns @c true if a matching service was found and @c false if no matching service or too many services were
         found. */
        bool findService(const char * criteria,
                         const bool   allowOnlyOneMatch = false);
        
    protected:
        
        /*! @brief Send a request to the service associated with the client.
         @param request The name of the request.
         @param parameters The required parameters for the request.
         @param response If non-@c NULL, where to store any response received.
         @returns @c true on a successful communication with the service and @c false otherwise. */
        bool send(const char *             request,
                  const yarp::os::Bottle & parameters,
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
        
        /*! @brief The port that the client uses for communication. */
        yarp::os::Port *      _clientPort;
        
        /*! @brief The name of the client port being used. */
        yarp::os::ConstString _clientPortName;

        /*! @brief The name of the service port being used. */
        yarp::os::ConstString _servicePortName;
        
        /*! @brief The root name for the client port. */
        char *                _basePortName;
        
        /*! @brief @c true if the client is connected to the service and @c false otherwise. */
        bool                  _connected;
        
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char                  _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
    }; // BaseClient
    
    /*! @brief Find one or more matching services that are registered with a running Service Registry service.
     @param criteria The matching conditions.
     @returns A (possibly empty) list of matching services. */
    yarp::os::Bottle FindMatchingServices(const char * criteria);
    
} // YarpPlusPlus

#endif // ! defined(YPPBASECLIENT_H_)
