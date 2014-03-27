//--------------------------------------------------------------------------------------
//
//  File:       YPPBaseRequestHandler.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for the minimal functionality required for a Yarp++
//              request handler.
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
//  Created:    2014-02-26
//
//--------------------------------------------------------------------------------------

#if (! defined(YPPBASEREQUESTHANDLER_H_))
/*! @brief Header guard. */
# define YPPBASEREQUESTHANDLER_H_ /* */

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
# include <yarp/os/ConnectionWriter.h>
# include <yarp/os/ConstString.h>
# include <yarp/os/Property.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the minimal functionality required for a Yarp++ request handler. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace YarpPlusPlus
{
    class RequestMap;

    /*! @brief A convenience class to provide function objects for requests. */
    class BaseRequestHandler
    {
    public:
        
        /*! @brief The constructor.
         @param request The name of the request. */
        BaseRequestHandler(const yarp::os::ConstString & request);
        
        /*! @brief The destructor. */
        virtual ~BaseRequestHandler(void);
        
        /*! @brief Fill in a set of aliases for the request.
         @param alternateNames Aliases for the request. */
        virtual void fillInAliases(StringVector & alternateNames) = 0;
        
        /*! @brief Fill in a description dictionary for the request.
         @param request The actual request name.
         @param info The dictionary to be filled in. */
        virtual void fillInDescription(const yarp::os::ConstString & request,
                                       yarp::os::Property &          info) = 0;
        
        /*! @brief Return the name of the request.
         @returns The name of the request. */
        inline yarp::os::ConstString name(void)
        const
        {
            return _name;
        } // name
        
        /*! @brief Process a request.
         @param request The actual request name.
         @param restOfInput The arguments to the operation.
         @param senderPort The name of the port used to send the input data.
         @param replyMechanism non-@c NULL if a reply is expected and @c NULL otherwise. */
        virtual bool processRequest(const yarp::os::ConstString & request,
                                    const yarp::os::Bottle &      restOfInput,
                                    const yarp::os::ConstString & senderPort,
                                    yarp::os::ConnectionWriter *  replyMechanism) = 0;
        
        /*! @brief Connect the handler to a map.
         @param owner The map that contains this handler. */
        void setOwner(RequestMap & owner);
        
    protected:
        
        /*! @brief The request map that 'owns' this handler. */
        RequestMap * _owner;
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseRequestHandler(const BaseRequestHandler & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        BaseRequestHandler & operator=(const BaseRequestHandler & other);
        
        /*! @brief The name of the request. */
        yarp::os::ConstString _name;
        
    }; // BaseRequestHandler
    
} // YarpPlusPlus

#endif // ! defined(YPPBASEREQUESTHANDLER_H_)
