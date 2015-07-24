//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseRequestHandler.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required for an m+m request
//              handler.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and / or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2014-02-26
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseRequestHandler_H_))
# define MpMBaseRequestHandler_H_ /* Header guard */

# include <m+m/m+mCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an m+m request handler. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        class RequestMap;
        class BaseService;
        
        /*! @brief A convenience class to provide function objects for requests. */
        class BaseRequestHandler
        {
        public :
            
            /*! @brief The constructor.
             @param request The name of the request.
             @param service The service associated with the request. */
            BaseRequestHandler(const YarpString & request,
                               BaseService &      service);
            
            /*! @brief The destructor. */
            virtual ~BaseRequestHandler(void);
            
            /*! @brief Fill in a set of aliases for the request.
             @param alternateNames Aliases for the request. */
            virtual void fillInAliases(YarpStringVector & alternateNames) = 0;
            
            /*! @brief Fill in a description dictionary for the request.
             @param request The actual request name.
             @param info The dictionary to be filled in. */
            virtual void fillInDescription(const YarpString &   request,
                                           yarp::os::Property & info) = 0;
            
            /*! @brief Return the name of the request.
             @returns The name of the request. */
            inline const YarpString & name(void)
            const
            {
                return _name;
            } // name
            
            /*! @brief Process a request.
             @param request The actual request name.
             @param restOfInput The arguments to the operation.
             @param senderChannel The name of the channel used to send the input data.
             @param replyMechanism non-@c NULL if a reply is expected and @c NULL otherwise. */
            virtual bool processRequest(const YarpString &           request,
                                        const yarp::os::Bottle &     restOfInput,
                                        const YarpString &           senderChannel,
                                        yarp::os::ConnectionWriter * replyMechanism) = 0;
            
            /*! @brief Send a simple OK response to a request.
             @param replyMechanism The destination for the response. */
            void sendOKResponse(yarp::os::ConnectionWriter * replyMechanism);
            
            /*! @brief Send a response to a request.
             @param replyMechanism The destination for the response. */
            void sendResponse(yarp::os::ConnectionWriter * replyMechanism);
            
            /*! @brief Connect the handler to a map.
             @param owner The map that contains this handler. */
            void setOwner(RequestMap & owner);
            
        protected :
            
            /*! @brief The object to use to hold the request response. */
            yarp::os::Bottle _response;
            
            /*! @brief The service that is associated with the request. */
            BaseService & _service;
            
            /*! @brief The request map that 'owns' this handler. */
            RequestMap * _owner;
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseRequestHandler);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The name of the request. */
            YarpString _name;
            
        }; // BaseRequestHandler
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseRequestHandler_H_)
